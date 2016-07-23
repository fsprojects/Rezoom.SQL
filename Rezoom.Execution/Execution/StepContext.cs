using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;

namespace Rezoom.Execution
{
    internal class StepContext
    {
        private readonly ServiceContext _serviceContext;
        private readonly IExecutionLog _executionLog;
        private readonly ResponseCache _cache;
        private readonly List<Func<Task>> _unsequenced = new List<Func<Task>>();
        private readonly Dictionary<object, List<Func<Task>>> _sequenceGroups
            = new Dictionary<object, List<Func<Task>>>();
        private readonly Dictionary<object, Func<DataResponse>> _deduped
            = new Dictionary<object, Func<DataResponse>>();

        public StepContext(ServiceContext serviceContext, IExecutionLog executionLog, ResponseCache cache)
        {
            _serviceContext = serviceContext;
            _executionLog = executionLog;
            _cache = cache;
        }

        private class PendingResult
        {
            private DataResponse _result;
            public DataResponse Get() => _result;
            public async Task Run(DataRequest request, IExecutionLog log, FSharpFunc<Unit, Task<object>> prepared)
            {
                try
                {
                    _result = DataResponse.NewRetrievalSuccess(await prepared.Invoke(null));
                }
                catch (Exception ex)
                {
                    _result = DataResponse.NewRetrievalException(ex);
                }
                log?.OnComplete(request, _result);
            }
        }

        private Func<DataResponse> AddRequestToRun(DataRequest request)
        {
            if (request.Mutation)
            {
                _cache.Invalidate(request.DataSource);
            }
            var eventual = new PendingResult();
            FSharpFunc<Unit, Task<object>> prepared;
            try
            {
                prepared = request.InternalPrepare(_serviceContext);
                _executionLog?.OnPrepare(request);
            }
            catch (Exception ex)
            {
                _executionLog?.OnPrepareFailure(ex);
                return () => DataResponse.NewRetrievalException(ex);
            }
            Func<Task> run = () => eventual.Run(request, _executionLog, prepared);
            if (!request.Parallelizable)
            {
                _unsequenced.Add(run);
            }
            else
            {
                var sequenceGroupId = request.SequenceGroup;
                if (sequenceGroupId == null)
                {
                    _unsequenced.Add(run);
                }
                else
                {
                    List<Func<Task>> sequenceGroup;
                    if (!_sequenceGroups.TryGetValue(sequenceGroupId, out sequenceGroup))
                    {
                        sequenceGroup = new List<Func<Task>>();
                        _sequenceGroups[sequenceGroupId] = sequenceGroup;
                    }
                    sequenceGroup.Add(run);
                }
            }
            return eventual.Get;
        }

        public Func<DataResponse> AddRequest(DataRequest request)
        {
            var identity = request.Identity;
            // If this request is not cachable, we have to run it.
            if (!request.Idempotent || identity == null) return AddRequestToRun(request);
            // Otherwise...
            var dataSource = request.DataSource;
            // Check for a cached result.
            object value = null;
            if (_cache.TryGetValue(dataSource, identity, ref value))
            {
                return () => DataResponse.NewRetrievalSuccess(value);
            }
            // Check for de-duplication of this request within this step.
            Func<DataResponse> existing;
            if (_deduped.TryGetValue(identity, out existing)) return existing;
            // Otherwise, we really need to run this request.
            var toRun = AddRequestToRun(request);
            _deduped[identity] = toRun;
            return () =>
            {
                var result = toRun();
                if (result.IsRetrievalSuccess)
                {
                    _cache.Store(dataSource, identity, ((DataResponse.RetrievalSuccess)result).Item);
                }
                return result;
            };
        }

        private static async Task ExecuteSequentialGroup(Task pending, IEnumerator<Func<Task>> rest)
        {
            await pending;
            while (rest.MoveNext())
            {
                await rest.Current();
            }
        }

        private static Task ExecuteSequentialGroup(IEnumerable<Func<Task>> tasks)
        {
            using (var enumerator = tasks.GetEnumerator())
            {
                while (enumerator.MoveNext())
                {
                    var task = enumerator.Current();
                    if (task.IsCompleted) continue;
                    return ExecuteSequentialGroup(task, enumerator);
                }
            }
            return Task.CompletedTask;
        }
        private static async Task Execute(Task[] tasks) => await Task.WhenAll(tasks);

        public Task Execute()
        {
            var tasks = new Task[_sequenceGroups.Values.Count + _unsequenced.Count];
            var i = 0;
            var allDone = true;
            foreach (var sgroup in _sequenceGroups.Values)
            {
                var task = ExecuteSequentialGroup(sgroup);
                tasks[i++] = task;
                allDone &= task.IsCompleted;
            }
            foreach (var unseq in _unsequenced)
            {
                var task = unseq();
                tasks[i++] = task;
                allDone &= task.IsCompleted;
            }
            return allDone ? Task.CompletedTask : Execute(tasks);
        }
    }
}
using System;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;

namespace Rezoom.Execution
{
    /// <summary>
    /// Handles execution of an <see cref="DataTask{TResult}"/> by stepping through it and running its pending
    /// <see cref="DataRequest"/>s with caching and deduplication.
    /// </summary>
    public class ExecutionContext : IDisposable
    {
        private readonly IExecutionLog _log;
        private readonly DefaultServiceContext _serviceContext;
        private readonly ResponseCache _responseCache = new ResponseCache();

        /// <summary>
        /// Create an execution context by giving it an <see cref="IServiceFactory"/> to provide
        /// services required by the <see cref="DataRequest"/>s that it'll be responsible for executing.
        /// </summary>
        /// <param name="serviceFactory"></param>
        /// <param name="log"></param>
        public ExecutionContext(ServiceFactory serviceFactory, IExecutionLog log = null)
        {
            _serviceContext = new DefaultServiceContext(serviceFactory);
            _log = log;
        }

        private async Task<DataTask<T>> ExecutePending<T>
            ( Batch<DataRequest> pending
            , FSharpFunc<Batch<DataResponse>, DataTask<T>> resume
            )
        {
            _log?.OnStepStart();
            _serviceContext.BeginStep();
            Batch<DataResponse> responses;
            try
            {
                var stepContext = new StepContext(_serviceContext, _log, _responseCache);
                var retrievals = pending.MapCS
                    (request => stepContext.AddRequest(request));
                await stepContext.Execute();
                responses = retrievals.MapCS(retrieve => retrieve());
            }
            finally
            {
                _serviceContext.EndStep();
                _log?.OnStepFinish();
            }
            return resume.Invoke(responses);
        }

        /// <summary>
        /// Asynchronously run the given <see cref="DataTask{TResult}"/> to completion.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="task"></param>
        /// <returns></returns>
        public async Task<T> Execute<T>(DataTask<T> task)
        {
            while (true)
            {
                if (task.IsStep)
                {
                    var step = (DataTask<T>.Step)task;
                    task = await ExecutePending(step.Item.Item1, step.Item.Item2);
                }
                else
                {
                    return ((DataTask<T>.Result)task).Item;
                }
            }
        }

        public TService GetService<TService>() => _serviceContext.GetService<TService>();
        public void Dispose() => _serviceContext.Dispose();
    }
}

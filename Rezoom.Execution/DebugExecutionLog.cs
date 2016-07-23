using System;
using System.Diagnostics;

namespace Rezoom.Execution
{
    /// <summary>
    /// An <see cref="IExecutionLog"/> that writes events to the debug output window.
    /// </summary>
    public class DebugExecutionLog : IExecutionLog
    {
        public void OnStepStart() => Debug.WriteLine("OnStepStart()");

        public void OnStepFinish() => Debug.WriteLine("OnStepFinish()");

        public void OnPrepare(Errand request)
            => Debug.WriteLine($"OnPrepare({request.Identity})");

        public void OnPrepareFailure(Exception exception)
            => Debug.WriteLine($"OnException({exception.Message})");

        public void OnComplete(Errand request, DataResponse response)
            => Debug.WriteLine($"OnComplete({request.Identity},{response})");
    }
}

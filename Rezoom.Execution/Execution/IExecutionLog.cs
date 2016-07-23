using System;

namespace Rezoom.Execution
{
    /// <summary>
    /// Receives all the data requests and their responses
    /// that occur during execution of a data task.
    /// </summary>
    /// <remarks>
    /// Execution log implementations are responsible for catching their own exceptions.
    /// They should never attempt to mutate the responses passed through them.
    /// </remarks>
    public interface IExecutionLog
    {
        /// <summary>
        /// Called when an execution step begins.
        /// </summary>
        void OnStepStart();
        /// <summary>
        /// Called when an execution step finishes.
        /// </summary>
        void OnStepFinish();
        /// <summary>
        /// Called when a data request is prepared for execution.
        /// </summary>
        /// <param name="request"></param>
        void OnPrepare(DataRequest request);
        /// <summary>
        /// Called when a data request's prepare method throws an exception.
        /// </summary>
        /// <param name="exception"></param>
        void OnPrepareFailure(Exception exception);
        /// <summary>
        /// Called when a data request has finished executing.
        /// </summary>
        /// <param name="request"></param>
        /// <param name="response"></param>
        void OnComplete(DataRequest request, DataResponse response);
    }
}

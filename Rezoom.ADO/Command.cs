using System;

namespace Rezoom.ADO
{
    public abstract class Command
    {
        protected Command(FormattableString text, bool mutation, bool idempotent)
        {
            Text = text;
            Mutation = mutation;
            Idempotent = idempotent;
        }

        public FormattableString Text { get; }
        public bool Mutation { get; }
        public bool Idempotent { get; }
        public abstract ResultSetProcessor Processor();
    }
    public abstract class Command<T> : Command
    {
        public abstract T ExtractResult(ResultSetProcessor processor);

        protected Command(FormattableString text, bool mutation, bool idempotent)
            : base(text, mutation, idempotent) { }
    }
}
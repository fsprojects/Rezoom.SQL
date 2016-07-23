using System;

namespace Rezoom.ADO
{
    public class Command
    {
        public Command(FormattableString text, bool mutation, bool idempotent)
        {
            Text = text;
            Mutation = mutation;
            Idempotent = idempotent;
        }
        public bool Mutation { get; }
        public bool Idempotent { get; }
        public FormattableString Text { get; }

        public static Command Query(FormattableString text, bool idempotent = true)
            => new Command(text, mutation: false, idempotent: idempotent);
        public static Command Mutate(FormattableString text, bool idempotent = false)
            => new Command(text, mutation: true, idempotent: idempotent);
    }
}
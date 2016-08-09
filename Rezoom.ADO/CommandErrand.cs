using System;
using System.Threading.Tasks;

namespace Rezoom.ADO
{
    public class CommandErrand<T> : CS.AsynchronousErrand<T>
    {
        private readonly Command<T> _command;

        public CommandErrand(Command<T> command)
        {
            _command = command;
        }

        public override object Identity => FormattableString.Invariant(_command.Text);
        public override object DataSource => typeof(CommandBatch);
        public override bool Mutation => _command.Mutation;
        public override bool Idempotent => _command.Idempotent;
        public override object SequenceGroup => typeof(CommandBatch);

        public override Func<Task<T>> Prepare(ServiceContext context)
            => context.GetService<CommandBatch>().Prepare(_command);
    }
}

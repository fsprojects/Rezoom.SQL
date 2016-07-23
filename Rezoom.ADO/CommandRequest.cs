using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Rezoom.ADO
{
    public class CommandRequest : CS.AsynchronousDataRequest<IReadOnlyList<CommandResponse>>
    {
        private readonly Command _command;

        public CommandRequest(Command command)
        {
            _command = command;
        }

        public override object Identity => FormattableString.Invariant(_command.Text);
        public override object DataSource => typeof(CommandBatch);
        public override bool Mutation => _command.Mutation;
        public override bool Idempotent => _command.Idempotent;
        public override object SequenceGroup => typeof(CommandBatch);

        public override Func<Task<IReadOnlyList<CommandResponse>>> Prepare(ServiceContext context)
            => context.GetService<CommandBatch>().Prepare(_command);
    }
}

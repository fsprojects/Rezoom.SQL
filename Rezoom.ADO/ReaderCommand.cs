using System;
using System.Data;
using StaticQL.Mapping;
using StaticQL.Mapping.CodeGeneration;

namespace Rezoom.ADO
{
    public class ReaderCommand<T> : Command<T>
    {
        public ReaderCommand(FormattableString text, bool mutation, bool idempotent)
            : base(text, mutation, idempotent)
        {
        }

        private class ReaderResultSetProcessor : ResultSetProcessor
        {
            private readonly EntityReader<T> _reader = ReaderTemplate<T>.Template().CreateReader();
            private Row _row;
            public override void BeginResultSet(IDataReader reader)
            {
                _reader.ProcessColumns(DataReader.columnMap(reader));
                _row = new DataReader.DataReaderRow(reader);
            }

            public override void ProcessRow() => _reader.Read(_row);
            public T ToEntity() => _reader.ToEntity();
        }

        public override T ExtractResult(ResultSetProcessor processor)
            => ((ReaderResultSetProcessor)processor).ToEntity();

        public override ResultSetProcessor Processor()
            => new ReaderResultSetProcessor();
    }
}

using System;
using System.Collections.Generic;
using System.Data;

namespace Rezoom.ADO
{
    public class RawCommand : Command<IReadOnlyList<RawResultSet>>
    {
        public RawCommand(FormattableString text, bool mutation, bool idempotent)
            : base(text, mutation, idempotent)
        {
        }

        private class RawResultSetProcessor : ResultSetProcessor
        {
            private readonly List<RawResultSet> _results = new List<RawResultSet>();
            private int _fieldCount;
            private IDataReader _reader;
            private string[] _columns;
            private List<object[]> _pending;

            private void CheckPending()
            {
                if (_pending == null) return;
                _results.Add(new RawResultSet(_columns, _pending));
                _pending = null;
            }

            public override void BeginResultSet(IDataReader reader)
            {
                CheckPending();
                _pending = new List<object[]>();
                _reader = reader;
                _fieldCount = reader.FieldCount;
                _columns = new string[_fieldCount];
                for (var i = 0; i < _columns.Length; i++)
                {
                    _columns[i] = reader.GetName(i);
                }
            }

            public override void ProcessRow()
            {
                var row = new object[_fieldCount];
                for (var i = 0; i < row.Length; i++)
                {
                    row[i] = _reader.GetValue(i);
                }
                _pending.Add(row);
            }

            public List<RawResultSet> Results()
            {
                CheckPending();
                return _results;
            }
        }

        public override ResultSetProcessor Processor() => new RawResultSetProcessor();

        public override IReadOnlyList<RawResultSet> ExtractResult(ResultSetProcessor processor)
            => ((RawResultSetProcessor)processor).Results();

        public static RawCommand Query(FormattableString text, bool idempotent = true)
            => new RawCommand(text, mutation: false, idempotent: idempotent);
        public static RawCommand Mutate(FormattableString text, bool idempotent = false)
            => new RawCommand(text, mutation: true, idempotent: idempotent);
    }
}
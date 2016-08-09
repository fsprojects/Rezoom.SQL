using System.Collections.Generic;

namespace Rezoom.ADO
{
    public class RawResultSet
    {
        public RawResultSet
            (IReadOnlyList<string> columnNames, IReadOnlyList<IReadOnlyList<object>> rows)
        {
            ColumnNames = columnNames;
            Rows = rows;
        }

        public IReadOnlyList<string> ColumnNames { get; }
        public IReadOnlyList<IReadOnlyList<object>> Rows { get; }

        public static readonly RawResultSet Empty
            = new RawResultSet(new string[0], new IReadOnlyList<object>[0]);
    }
}
using System;
using System.Collections.Generic;

namespace Rezoom.ADO.Materialization
{
    public class ColumnMap
    {
        private readonly Dictionary<string, int> _columnIndices =
            new Dictionary<string, int>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, ColumnMap> _subMaps =
            new Dictionary<string, ColumnMap>(StringComparer.OrdinalIgnoreCase);

        private ColumnMap GetOrCreateSubMap(string name)
        {
            ColumnMap val;
            if (_subMaps.TryGetValue(name, out val)) return val;
            val = new ColumnMap();
            _subMaps[name] = val;
            return val;
        }

        private void SetColumnIndex(string name, int index) => _columnIndices[name] = index;

        private void Load(IReadOnlyList<string> columnNames)
        {
            var root = this;
            var current = this;
            for (var i = 0; i < columnNames.Count; i++)
            {
                var path = columnNames[i].Split('.', '$');
                if (path.Length == 1) current.SetColumnIndex(path[0], i);
                else
                {
                    current = root;
                    for (var j = 0; j < path.Length - 1; j++)
                    {
                        current = current.GetOrCreateSubMap(path[j]);
                    }
                    current.SetColumnIndex(path[path.Length - 1], i);
                }
            }
        }

        public int ColumnIndex(string propertyName)
        {
            int idx;
            return _columnIndices.TryGetValue(propertyName, out idx) ? idx : -1;
        }

        public ColumnMap SubMap(string propertyName)
        {
            ColumnMap sub;
            return _subMaps.TryGetValue(propertyName, out sub) ? sub : null;
        }

        internal static ColumnMap Parse(IReadOnlyList<string> columnNames)
        {
            var map = new ColumnMap();
            map.Load(columnNames);
            return map;
        }
    }
}
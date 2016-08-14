using System.Data;

namespace Rezoom.ADO
{
    internal class MultiCommandParameter
    {
        public MultiCommandParameter(string name, DbType type, object value)
        {
            Name = name;
            Type = type;
            Value = value;
        }

        public string Name { get; }
        public DbType Type { get; }
        public object Value { get; }
    }
}
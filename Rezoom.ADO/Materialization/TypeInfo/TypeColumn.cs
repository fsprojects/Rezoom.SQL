using System;
using System.Reflection;

namespace Rezoom.ADO.Materialization.TypeInfo
{
    public class TypeColumn
    {
        public TypeColumn(string name, Type type, MethodInfo setter)
        {
            Name = name;
            Type = type;
            Setter = setter;
        }

        public string Name { get; }
        public Type Type { get; }
        public MethodInfo Setter { get; }
    }
}
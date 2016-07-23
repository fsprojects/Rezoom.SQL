using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Rezoom.ADO.Materialization.TypeInfo
{
    public static class TypeProfile<T>
    {
        public static readonly TypeProfile Profile = TypeProfile.OfType(typeof(T));
    }
    public class TypeProfile
    {
        private readonly Dictionary<string, TypeColumn> _columnsByNameCI;
        private TypeProfile(Type type, ConstructorInfo primaryConstructor, IReadOnlyList<TypeColumn> typeColumns)
        {
            Type = type;
            PrimaryConstructor = primaryConstructor;
            Columns = typeColumns;
            _columnsByNameCI = typeColumns.ToDictionary(t => t.Name, StringComparer.OrdinalIgnoreCase);
        }

        public Type Type { get; }
        public ConstructorInfo PrimaryConstructor { get; }
        public IReadOnlyList<TypeColumn> Columns { get; }

        public TypeColumn KeyColumn
        {
            get
            {
                TypeColumn found;
                if (_columnsByNameCI.TryGetValue("id", out found)) return found;
                if (_columnsByNameCI.TryGetValue(Type.Name + "id", out found)) return found;
                if (_columnsByNameCI.TryGetValue(Type.Name + "_id", out found)) return found;
                return null;
            }
        }

        public static TypeProfile OfType(Type type)
        {
            var settableProperties = type.GetProperties()
                .Where(p => p.CanWrite)
                .Select(p => new
                {
                    Property = p,
                    Setter = p.GetSetMethod(nonPublic: false)
                })
                .Where(p => p.Setter != null)
                .ToList();
            var constructors = type.GetConstructors().Select(c => new
            {
                Constructor = c,
                Parameters = c.GetParameters(),
            }).ToList();
            var longestConstructor = constructors.OrderByDescending(c => c.Parameters.Length).FirstOrDefault();
            if (longestConstructor == null) throw new ArgumentException("Type has no public constructors", nameof(type));
            var defaultConstructor = constructors.FirstOrDefault(c => c.Parameters.Length == 0);
            if (defaultConstructor == null || longestConstructor.Parameters.Length >= settableProperties.Count)
            {
                var columns = longestConstructor.Parameters
                    .Select(p => new TypeColumn(p.Name, p.ParameterType, null))
                    .ToList();
                return new TypeProfile(type, longestConstructor.Constructor, columns);
            }
            else
            {
                var columns = settableProperties
                    .Select(p => new TypeColumn(p.Property.Name, p.Property.PropertyType, p.Setter))
                    .ToList();
                return new TypeProfile(type, defaultConstructor.Constructor, columns);
            }
        }
    }
}
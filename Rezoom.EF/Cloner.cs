using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Rezoom.EF
{
    internal class Cloner<TEntity> where TEntity : new()
    {
        private static PropertyInfo[] ClonableProperties()
        {
            var props = typeof(TEntity).GetProperties();
            var names = new HashSet<string>(props.Select(p => p.Name), StringComparer.OrdinalIgnoreCase);
            return props.Where(p
                => p.CanRead
                    && p.CanWrite
                    && !names.Contains(p.Name + "Id")) // don't clone navigation properties
                .ToArray();
        }

        private readonly PropertyInfo[] _cloneProperties = ClonableProperties();

        public TEntity Clone(TEntity entity)
        {
            var newEntity = new TEntity();
            foreach (var prop in _cloneProperties)
            {
                prop.SetValue(newEntity, prop.GetValue(entity));
            }
            return newEntity;
        }

        public static readonly Cloner<TEntity> Instance = new Cloner<TEntity>();
    }
}
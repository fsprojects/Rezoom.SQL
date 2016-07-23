using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using Microsoft.FSharp.Collections;

namespace Rezoom.ADO.Materialization
{
    public static class ManyNavConverter
    {
        private static readonly HashSet<Type> SupportedGenerics =
            new HashSet<Type>(new[]
            {
                typeof(IEnumerable<>),
                typeof(IDictionary<,>),
                typeof(IReadOnlyDictionary<,>),
                typeof(ICollection<>),
                typeof(IReadOnlyCollection<>),
                typeof(IList<>),
                typeof(IReadOnlyList<>),
                typeof(List<>),
                typeof(Dictionary<,>),
                typeof(ReadOnlyCollection<>),
                typeof(FSharpList<>),
                typeof(FSharpMap<,>),
                typeof(FSharpSet<>),
            });
        public static Type IsMany(Type collectionType)
        {
            if (collectionType.IsArray) return collectionType.GetElementType();
            if (!collectionType.IsConstructedGenericType) return null;
            var def = collectionType.GetGenericTypeDefinition();
            // TODO: this is a hack, but happens to work for all the supported types
            return SupportedGenerics.Contains(def) ? collectionType.GetGenericArguments().Last() : null;
        }
    }
    public static class ManyNavConverter<TKey, TEntity>
    {
        public static Dictionary<TKey, TEntity> ToDictionary(Dictionary<TKey, IRowReader<TEntity>> collection)
        {
            var dictionary = new Dictionary<TKey, TEntity>(collection.Count);
            foreach (var kv in collection)
            {
                dictionary[kv.Key] = kv.Value.ToEntity();
            }
            return dictionary;
        }
        public static IDictionary<TKey, TEntity> ToIDictionary(Dictionary<TKey, IRowReader<TEntity>> collection)
            => ToDictionary(collection);
        public static IReadOnlyDictionary<TKey, TEntity> ToIReadOnlyDictionary
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToDictionary(collection);

        public static TEntity[] ToArray(Dictionary<TKey, IRowReader<TEntity>> collection)
        {
            var i = 0;
            var arr = new TEntity[collection.Count];
            foreach (var element in collection.Values)
            {
                arr[i++] = element.ToEntity();
            }
            return arr;
        }

        public static IEnumerable<TEntity> ToIEnumerable
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToArray(collection);

        public static ICollection<TEntity> ToICollection
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToArray(collection);

        public static IReadOnlyCollection<TEntity> ToIReadOnlyCollection
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToArray(collection);

        public static IList<TEntity> ToIList
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToArray(collection);

        public static IReadOnlyList<TEntity> ToIReadOnlyList
            (Dictionary<TKey, IRowReader<TEntity>> collection) => ToArray(collection);

        public static ReadOnlyCollection<TEntity> ToReadOnlyCollection
            (Dictionary<TKey, IRowReader<TEntity>> collection) => new ReadOnlyCollection<TEntity>(ToArray(collection));

        public static List<TEntity> ToList(Dictionary<TKey, IRowReader<TEntity>> collection)
        {
            var list = new List<TEntity>(collection.Count);
            foreach (var element in collection.Values)
            {
                list.Add(element.ToEntity());
            }
            return list;
        }

        public static FSharpList<TEntity> ToFSharpList(Dictionary<TKey, IRowReader<TEntity>> collection)
        {
            var list = FSharpList<TEntity>.Empty;
            foreach (var element in collection.Values)
            {
                list = FSharpList<TEntity>.Cons(element.ToEntity(), list);
            }
            return list;
        }

        public static FSharpMap<TKey, TEntity> ToFSharpMap(Dictionary<TKey, IRowReader<TEntity>> collection)
            => MapModule.OfSeq(collection.Select(kv => Tuple.Create(kv.Key, kv.Value.ToEntity())));

        public static FSharpSet<TEntity> ToFSharpSet(Dictionary<TKey, IRowReader<TEntity>> collection)
            => SetModule.OfSeq(collection.Values.Select(v => v.ToEntity()));

        private static readonly Dictionary<Type, MethodInfo> Converters =
            typeof(ManyNavConverter<TKey, TEntity>)
                .GetMethods(BindingFlags.Public | BindingFlags.Static)
                .Where(m =>
                {
                    var pars = m.GetParameters();
                    return pars.Length == 1
                        && pars[0].ParameterType == typeof(Dictionary<TKey, IRowReader<TEntity>>)
                        && m.ReturnType != typeof(void);
                }).ToDictionary(m => m.ReturnType);

        public static bool IsManyNav(Type targetType) => Converters.ContainsKey(targetType);

        public static MethodInfo ToType(Type targetType)
        {
            MethodInfo converter;
            if (Converters.TryGetValue(targetType, out converter)) return converter;
            throw new NotSupportedException($"Can't convert to {targetType}");
        }
    }
}
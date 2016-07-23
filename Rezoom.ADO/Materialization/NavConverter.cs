using System.Collections.Generic;

namespace Rezoom.ADO.Materialization
{
    public static class NavConverter<T>
    {
        public static T[] ToArray(ICollection<IRowReader<T>> collection)
        {
            var i = 0;
            var arr = new T[collection.Count];
            foreach (var element in collection)
            {
                arr[i++] = element.ToEntity();
            }
            return arr;
        }
    }
}
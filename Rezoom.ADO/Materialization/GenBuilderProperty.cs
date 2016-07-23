using System;
using Rezoom.ADO.Materialization.GenBuilderProperties;

namespace Rezoom.ADO.Materialization
{
    internal static class GenBuilderProperty
    {
        public static IGenBuilderProperty GetProperty(string name, Type propertyType)
        {
            if (PrimitiveConverter.IsPrimitive(propertyType))
            {
                return new PrimitiveGenBuilderProperty(name, propertyType);
            }
            var elementType = ManyNavConverter.IsMany(propertyType);
            if (elementType != null)
            {
                return new ManyNavGenBuilderProperty(name, elementType, propertyType);
            }
            return new SingleNavGenBuilderProperty(name, propertyType);
        }
    }
}

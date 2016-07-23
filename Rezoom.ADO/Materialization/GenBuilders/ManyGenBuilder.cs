using System;
using System.Collections.Generic;
using System.Reflection.Emit;
using Rezoom.ADO.Materialization.GenBuilderProperties;

namespace Rezoom.ADO.Materialization.GenBuilders
{
    internal class ManyGenBuilder : IGenBuilder
    {
        public ManyGenBuilder(Type collectionType)
        {
            var element = ManyNavConverter.IsMany(collectionType);
            if (element == null) throw new NotSupportedException
                ($"{collectionType} is not a supported collection type");
            Properties = new IGenBuilderProperty[]
            {
                new ManyNavGenBuilderProperty(fieldName: null, entityType: element, collectionType: collectionType)
            };
        }
        public IReadOnlyList<IGenBuilderProperty> Properties { get; }
        public void InstallConstructor(ILGenerator il)
        {
            // the single property value *is* the collection, so no need to mess with the stack
        }
    }
}

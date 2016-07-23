using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization.GenBuilders
{
    /// <summary>
    /// Implements IGenBuilder for a type we'll initialize by calling its default constructor, then
    /// assigning its writable properties (even those with private setters).
    /// </summary>
    internal class PropertyAssignmentGenBuilder : IGenBuilder
    {
        private readonly Type _targetType;
        private readonly ConstructorInfo _parameterlessConstructor;
        private readonly List<PropertyInfo> _props;
        public PropertyAssignmentGenBuilder(Type type)
        {
            _targetType = type;
            _parameterlessConstructor = type.GetConstructor(Type.EmptyTypes);
            _props = type.GetProperties()
                .Where(p => p.CanWrite)
                .ToList();
            Properties = _props
                .Select(p => GenBuilderProperty.GetProperty(p.Name, p.PropertyType))
                .ToList();
        }

        public IReadOnlyList<IGenBuilderProperty> Properties { get; }
        public void InstallConstructor(ILGenerator il)
        {
            // initialize the object
            il.Emit(OpCodes.Newobj, _parameterlessConstructor);
            // now we have:
            // prop1, prop2, prop3, object
            // calling the setters is going to require a bit of stack-shuffling
            // since for each one, we need:
            // object, prop
            var oloc = il.DeclareLocal(_targetType);
            il.Emit(OpCodes.Stloc, oloc); // pop object to local
            for (var i = _props.Count - 1; i >= 0; i--)
            {
                var prop = _props[i];
                var ploc = il.DeclareLocal(prop.PropertyType);
                il.Emit(OpCodes.Stloc, ploc); // pop property to local
                il.Emit(OpCodes.Ldloc, oloc); // push object
                il.Emit(OpCodes.Ldloc, ploc); // push property

                var setter = prop.GetSetMethod(nonPublic: true);
                // pop both by calling setter
                if (setter.IsVirtual)
                {
                    il.Emit(OpCodes.Constrained, _targetType);
                    il.Emit(OpCodes.Callvirt, setter);
                }
                else
                {
                    il.Emit(OpCodes.Call, setter);
                }
            }
            il.Emit(OpCodes.Ldloc, oloc); // push object
        }
    }
}
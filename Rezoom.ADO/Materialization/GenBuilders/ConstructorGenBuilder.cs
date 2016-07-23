using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization.GenBuilders
{
    /// <summary>
    /// Implements IGenBuilder for a type we'll initialize by calling its constructor with parameters.
    /// </summary>
    internal class ConstructorGenBuilder : IGenBuilder
    {
        private readonly ConstructorInfo _constructor;

        public ConstructorGenBuilder(ConstructorInfo constructor)
        {
            _constructor = constructor;
            Properties =
                _constructor.GetParameters()
                    .Select(p => GenBuilderProperty.GetProperty(p.Name, p.ParameterType))
                    .ToList();
        }

        public IReadOnlyList<IGenBuilderProperty> Properties { get; }

        public void InstallConstructor(ILGenerator il) => il.Emit(OpCodes.Newobj, _constructor);
    }
}
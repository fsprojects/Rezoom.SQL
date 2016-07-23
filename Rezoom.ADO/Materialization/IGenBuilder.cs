using System.Collections.Generic;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization
{
    internal interface IGenBuilder
    {
        /// <summary>
        /// Get the properties of this type in constructor order.
        /// </summary>
        IReadOnlyList<IGenBuilderProperty> Properties { get; }
        /// <summary>
        /// Assuming all the values of the properties are on the stack in constructor order,
        /// add the logic to call the constructor (and perform any post-constructor assignments).
        /// 
        /// Should effectively pop all the property values and push the constructed object.
        /// </summary>
        /// <remarks>
        /// Used for the generated IBuilder's <c>Materialize()</c> method.
        /// </remarks>
        /// <param name="il"></param>
        void InstallConstructor(ILGenerator il);
    }
}
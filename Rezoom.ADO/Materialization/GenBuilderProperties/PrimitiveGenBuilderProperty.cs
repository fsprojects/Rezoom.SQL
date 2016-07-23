using System;
using System.Reflection;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization.GenBuilderProperties
{
    /// <summary>
    /// Implements IGenBuilderProperty for a primitive field -- for example, one of type int, string, or Guid.
    /// </summary>
    internal class PrimitiveGenBuilderProperty : IGenBuilderProperty
    {
        private readonly string _fieldName;
        private readonly Type _fieldType;
        private readonly bool _nonNull;

        /// <summary>
        /// The field that stores the value for this property.
        /// </summary>
        private FieldBuilder _value;
        /// <summary>
        /// The boolean field that stores whether or not we've loaded the value for this property yet.
        /// </summary>
        private FieldBuilder _seen;
        /// <summary>
        /// The field that stores the column index for this property.
        /// </summary>
        private FieldBuilder _columnIndex;

        public PrimitiveGenBuilderProperty(string fieldName, Type fieldType)
        {
            _fieldName = fieldName;
            _fieldType = fieldType;
            _nonNull = fieldType.IsValueType &&
                (!fieldType.IsGenericType || fieldType.GetGenericTypeDefinition() != typeof(Nullable<>));
        }

        public bool Singular => true;

        public void InstallFields(TypeBuilder type, ILGenerator constructor)
        {
            _value = type.DefineField("_dr_" + _fieldName, _fieldType, FieldAttributes.Private);
            _seen = type.DefineField("_dr_seen_" + _fieldName, typeof(bool), FieldAttributes.Private);
            _columnIndex = type.DefineField("_dr_col_" + _fieldName, typeof(int), FieldAttributes.Private);
        }

        public void InstallProcessingLogic(GenProcessColumnMapContext cxt)
        {
            var il = cxt.IL;
            il.Emit(OpCodes.Dup); // dup this
            il.Emit(OpCodes.Ldloc, cxt.ColumnMap);
            il.Emit(OpCodes.Ldstr, _fieldName);
            il.Emit(OpCodes.Callvirt, typeof(ColumnMap).GetMethod(nameof(ColumnMap.ColumnIndex)));
            il.Emit(OpCodes.Stfld, _columnIndex);
        }

        public void InstallProcessingLogic(GenProcessRowContext cxt)
        {
            var il = cxt.IL;
            var skipOnNull = _nonNull ? cxt.SkipSingularProperties : il.DefineLabel();

            // First check if we can skip singular properties
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Ldfld, _seen);
            il.Emit(OpCodes.Brtrue, cxt.SkipSingularProperties);
            {
                // If not, attempt to load the value.
                // Load the row array
                il.Emit(OpCodes.Ldloc, cxt.Row);
                // Get the column index
                il.Emit(OpCodes.Ldloc, cxt.This);
                il.Emit(OpCodes.Ldfld, _columnIndex);
                // Load the value from the array
                il.Emit(OpCodes.Ldelem_Ref);
                var obj = il.DeclareLocal(typeof(object));
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Stloc, obj);
                // If the value is null, we can skip this property
                // ... in fact, if the value is null but we're non-nullable, we can skip all singulars
                il.Emit(OpCodes.Brfalse, skipOnNull);
                {
                    // Convert and save the object
                    il.Emit(OpCodes.Dup); // dup "this" instance
                    il.Emit(OpCodes.Ldloc, obj);
                    il.Emit(OpCodes.Call, PrimitiveConverter.ToType(_fieldType));
                    il.Emit(OpCodes.Stfld, _value);
                    // Set seen to true
                    il.Emit(OpCodes.Dup); // dup "this" instance
                    il.Emit(OpCodes.Ldc_I4_1); // 1 (true)
                    il.Emit(OpCodes.Stfld, _seen);
                }
            }
            if (_nonNull) return;
            il.MarkLabel(skipOnNull);
        }

        public void InstallPushValue(GenInstanceMethodContext cxt)
        {
            var il = cxt.IL;
            il.Emit(OpCodes.Ldloc, cxt.This);
            il.Emit(OpCodes.Ldfld, _value);
        }
    }
}

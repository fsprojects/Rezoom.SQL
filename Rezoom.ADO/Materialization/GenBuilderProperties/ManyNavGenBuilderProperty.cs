using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization.GenBuilderProperties
{
    internal class ManyNavGenBuilderProperty : NavGenBuilderProperty
    {
        private readonly Type _dictionaryType;
        private readonly Type _collectionType;

        private FieldBuilder _dict;

        public ManyNavGenBuilderProperty(string fieldName, Type entityType, Type collectionType)
            : base(fieldName, entityType)
        {
            _collectionType = collectionType;
            _dictionaryType = typeof(Dictionary<,>).MakeGenericType(KeyColumn.Type, EntityReaderType);
        }

        public override void InstallFields(TypeBuilder type, ILGenerator constructor)
        {
            base.InstallFields(type, constructor);
            _dict = type.DefineField("_dr_dict_" + FieldName, _dictionaryType, FieldAttributes.Private);
            var cons = _dictionaryType.GetConstructor(Type.EmptyTypes);
            if (cons == null) throw new Exception("Unexpected lack of default constructor on dictionary type");

            constructor.Emit(OpCodes.Dup); // dup "this"
            constructor.Emit(OpCodes.Newobj, cons); // new dictionary
            constructor.Emit(OpCodes.Stfld, _dict); // assign field
        }

        public override void InstallProcessingLogic(GenProcessRowContext cxt)
        {
            var il = cxt.IL;
            var skip = il.DefineLabel();
            var subProcess = il.DefineLabel();
            var keyRaw = il.DeclareLocal(typeof(object));
            var key = il.DeclareLocal(KeyColumn.Type);
            var entReader = il.DeclareLocal(EntityReaderType);
            il.Emit(OpCodes.Dup); // this, this
            il.Emit(OpCodes.Ldfld, SubColumnMap); // this, cmap
            il.Emit(OpCodes.Brfalse, skip); // skip if we have no column map (for recursive case)

            // get the key value from the row
            il.Emit(OpCodes.Ldloc, cxt.Row); // this, row
            il.Emit(OpCodes.Ldloc, cxt.This); // this, row, this
            il.Emit(OpCodes.Ldfld, KeyColumnIndex); // this, row, index
            il.Emit(OpCodes.Ldelem_Ref); // this, rval
            // store it in a local
            il.Emit(OpCodes.Dup); // this, rval, rval
            il.Emit(OpCodes.Stloc, keyRaw); // this, rval
            // if our id is null, bail
            il.Emit(OpCodes.Brfalse, skip);
            {
                // stack clean (this at top)
                il.Emit(OpCodes.Dup); // this, this
                il.Emit(OpCodes.Ldfld, _dict); // this, dict
                il.Emit(OpCodes.Ldloc, keyRaw); // this, dict, rval
                il.Emit(OpCodes.Call, PrimitiveConverter.ToType(KeyColumn.Type)); // this, dict, key
                il.Emit(OpCodes.Dup); // this, dict, key, key
                il.Emit(OpCodes.Stloc, key); // this, dict, key
                il.Emit(OpCodes.Ldloca, entReader); // this, dict, key, &reader
                il.Emit(OpCodes.Call, _dictionaryType.GetMethod(nameof(Dictionary<object, object>.TryGetValue)));
                // this, gotv
                // if we've got one, skip to sub-processing the row
                il.Emit(OpCodes.Brtrue, subProcess);
                {
                    // otherwise, make one
                    // stack clean (this at top)
                    il.Emit(OpCodes.Ldsfld, EntityReaderStaticTemplateType.GetField
                        (nameof(RowReaderTemplate<object>.Template)));
                    // this, template
                    il.Emit(OpCodes.Callvirt, EntityReaderTemplateType.GetMethod
                        (nameof(IRowReaderTemplate<object>.CreateReader)));
                    // this, newreader
                    il.Emit(OpCodes.Dup);
                    // this, newreader, newreader
                    il.Emit(OpCodes.Stloc, entReader);
                    // this, newreader

                    // process column map
                    il.Emit(OpCodes.Ldloc, cxt.This); // this, newreader, this
                    il.Emit(OpCodes.Ldfld, SubColumnMap); // this, newreader, columnmap
                    il.Emit(OpCodes.Callvirt, EntityReaderType.GetMethod
                        (nameof(IRowReader<object>.ProcessColumnMap)));

                    // save in dictionary
                    // stack clean (this at top)
                    il.Emit(OpCodes.Dup); // this, this
                    il.Emit(OpCodes.Ldfld, _dict); // this, dict
                    il.Emit(OpCodes.Ldloc, key); // this, dict, key
                    il.Emit(OpCodes.Ldloc, entReader); // this, dict, key, reader
                    il.Emit(OpCodes.Call, _dictionaryType.GetMethod
                        (nameof(Dictionary<object, object>.Add)));
                    // stack clean (this at top)
                }
                il.MarkLabel(subProcess);
                // have the entity reader process the row
                il.Emit(OpCodes.Ldloc, entReader); // this, reader
                il.Emit(OpCodes.Ldloc, cxt.Row); // this, reader, row
                il.Emit(OpCodes.Callvirt, EntityReaderType.GetMethod(nameof(IRowReader<object>.ProcessRow)));
                // this
            }
            il.MarkLabel(skip);
        }

        public override void InstallPushValue(GenInstanceMethodContext cxt)
        {
            var il = cxt.IL;

            il.Emit(OpCodes.Ldloc, cxt.This); // this
            il.Emit(OpCodes.Ldfld, _dict); // dict
            var manyNavConverter = typeof(ManyNavConverter<,>)
                .MakeGenericType(KeyColumn.Type, EntityType);
            var conversion =
                (MethodInfo)
                manyNavConverter.GetMethod(nameof(ManyNavConverter<object, object>.ToType))
                    .Invoke(null, new object[] { _collectionType });
            il.Emit(OpCodes.Call, conversion);
        }
    }
}

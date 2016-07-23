using System;
using System.Reflection;
using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization.GenBuilderProperties
{
    internal class SingleNavGenBuilderProperty : NavGenBuilderProperty
    {
        private FieldBuilder _reader;

        public SingleNavGenBuilderProperty(string fieldName, Type entityType)
            : base(fieldName, entityType)
        {
        }

        public override void InstallFields(TypeBuilder type, ILGenerator constructor)
        {
            base.InstallFields(type, constructor);
            _reader = type.DefineField("_dr_single_" + FieldName, EntityReaderType, FieldAttributes.Private);
        }

        public override void InstallProcessingLogic(GenProcessRowContext cxt)
        {
            var il = cxt.IL;
            var skip = il.DefineLabel();
            var read = il.DefineLabel();
            var localReader = il.DeclareLocal(EntityReaderType);

            il.Emit(OpCodes.Dup); // this, this
            il.Emit(OpCodes.Ldfld, _reader); // this, reader
            il.Emit(OpCodes.Dup); // this, reader, reader
            il.Emit(OpCodes.Stloc, localReader); // this, reader
            il.Emit(OpCodes.Brtrue_S, read);
            {
                // ok, if we don't have a reader, let's check our id column and see if we can get one going
                il.Emit(OpCodes.Ldloc, cxt.Row); // this, row
                il.Emit(OpCodes.Ldloc, cxt.This); // this, row, this
                il.Emit(OpCodes.Ldfld, KeyColumnIndex); // this, row, kidx
                il.Emit(OpCodes.Ldelem_Ref); // this, kval
                il.Emit(OpCodes.Brfalse, skip); // if we have a null value, skip
                // ok, we definitely have an id, but no reader yet -- let's make one
                il.Emit(OpCodes.Ldsfld, EntityReaderStaticTemplateType.GetField
                    (nameof(RowReaderTemplate<object>.Template)));
                // this, template
                il.Emit(OpCodes.Callvirt, EntityReaderTemplateType.GetMethod
                    (nameof(IRowReaderTemplate<object>.CreateReader)));
                // this, newreader
                il.Emit(OpCodes.Dup); // this, newreader, newreader
                il.Emit(OpCodes.Stloc, localReader); // this, newreader
                il.Emit(OpCodes.Dup); // this, newreader, newreader
                il.Emit(OpCodes.Ldloc, cxt.This); // this, newreader, newreader, this
                il.Emit(OpCodes.Ldfld, SubColumnMap); // this, newreader, newreader, submap
                il.Emit(OpCodes.Callvirt, EntityReaderType.GetMethod(nameof(IRowReader<object>.ProcessColumnMap)));
                // this, newreader
                il.Emit(OpCodes.Stfld, _reader);
                il.Emit(OpCodes.Ldloc, cxt.This);
                // this
            }
            il.MarkLabel(read);
            il.Emit(OpCodes.Ldloc, localReader); // this, reader
            il.Emit(OpCodes.Ldloc, cxt.Row); // this, reader, row
            il.Emit(OpCodes.Callvirt, EntityReaderType.GetMethod(nameof(IRowReader<object>.ProcessRow))); // this
            il.MarkLabel(skip);
        }

        public override void InstallPushValue(GenInstanceMethodContext cxt)
        {
            var il = cxt.IL;
            var val = il.DefineLabel();
            var done = il.DefineLabel();
            il.Emit(OpCodes.Ldloc, cxt.This);
            il.Emit(OpCodes.Ldfld, _reader); // reader
            il.Emit(OpCodes.Dup); // reader, reader
            il.Emit(OpCodes.Brtrue_S, val);

            {
                il.Emit(OpCodes.Pop);
                if (EntityType.IsValueType)
                {
                    var defaultValue = il.DeclareLocal(EntityType);
                    il.Emit(OpCodes.Ldloca, defaultValue);
                    il.Emit(OpCodes.Initobj, EntityType);
                    il.Emit(OpCodes.Ldloc, defaultValue);
                }
                else
                {
                    il.Emit(OpCodes.Ldnull);
                }
                il.Emit(OpCodes.Br_S, done);
            }

            il.MarkLabel(val); // reader

            il.Emit(OpCodes.Callvirt, EntityReaderType.GetMethod
                (nameof(IRowReader<object>.ToEntity))); // entity

            il.MarkLabel(done);
        }
    }
}
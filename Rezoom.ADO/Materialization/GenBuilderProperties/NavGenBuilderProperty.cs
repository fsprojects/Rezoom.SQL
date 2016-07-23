using System;
using System.Reflection;
using System.Reflection.Emit;
using Rezoom.ADO.Materialization.TypeInfo;

namespace Rezoom.ADO.Materialization.GenBuilderProperties
{
    internal abstract class NavGenBuilderProperty : IGenBuilderProperty
    {
        protected readonly Type EntityReaderStaticTemplateType;
        protected readonly Type EntityReaderTemplateType;
        protected readonly Type EntityReaderType;
        protected readonly Type EntityType;
        protected readonly string FieldName;
        protected readonly TypeColumn KeyColumn;

        protected NavGenBuilderProperty(string fieldName, Type entityType)
        {
            FieldName = fieldName;
            EntityType = entityType;
            EntityReaderType = typeof(IRowReader<>).MakeGenericType(entityType);
            EntityReaderStaticTemplateType = typeof(RowReaderTemplate<>).MakeGenericType(entityType);
            EntityReaderTemplateType = typeof(IRowReaderTemplate<>).MakeGenericType(entityType);
            var key = typeof(TypeProfile<>).MakeGenericType(entityType)
                .GetField(nameof(TypeProfile<object>.Profile))
                .GetValue(null) as TypeProfile;
            if (key == null) throw new NullReferenceException("Unexpected null type profile");
            KeyColumn = key.KeyColumn;
            if (KeyColumn == null) throw new InvalidOperationException($"Type {entityType} has no key column");
        }

        protected FieldBuilder SubColumnMap;
        protected FieldBuilder KeyColumnIndex;

        public bool Singular => false;

        public virtual void InstallFields(TypeBuilder type, ILGenerator constructor)
        {
            SubColumnMap = type.DefineField("_dr_cmap_" + FieldName, typeof(ColumnMap), FieldAttributes.Private);
            KeyColumnIndex = type.DefineField("_dr_kcol_" + FieldName, typeof(int), FieldAttributes.Private);
        }

        public virtual void InstallProcessingLogic(GenProcessColumnMapContext cxt)
        {
            var il = cxt.IL;
            var skip = il.DefineLabel();
            var done = il.DefineLabel();
            il.Emit(OpCodes.Dup); // this, this
            il.Emit(OpCodes.Dup); // this, this, this
            // Get submap for this nav property
            il.Emit(OpCodes.Dup); // this, this, this, this
            il.Emit(OpCodes.Ldloc, cxt.ColumnMap); // this, this, this, this, colmap
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Brfalse_S, skip);
            {
                if (FieldName != null)
                {
                    il.Emit(OpCodes.Ldstr, FieldName); // this, this, this, this colmap, fieldname
                    il.Emit(OpCodes.Call, typeof(ColumnMap).GetMethod(nameof(ColumnMap.SubMap)));
                }
                // this, this, this, this, submap
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Brfalse_S, skip);
                // Set column map field to submap
                il.Emit(OpCodes.Stfld, SubColumnMap); // this, this, this
                il.Emit(OpCodes.Ldfld, SubColumnMap); // this, this, submap
                                                      // Get key column index from submap
                il.Emit(OpCodes.Ldstr, KeyColumn.Name); // this, this, submap, keyfield
                il.Emit(OpCodes.Call, typeof(ColumnMap).GetMethod(nameof(ColumnMap.ColumnIndex)));
                // this, this, keyindex
                il.Emit(OpCodes.Stfld, KeyColumnIndex);
                il.Emit(OpCodes.Br_S, done);
            }
            il.MarkLabel(skip);
            {
                // this, this, this, this, submap
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Pop);
            }
            il.MarkLabel(done);
        }

        public abstract void InstallProcessingLogic(GenProcessRowContext cxt);

        public abstract void InstallPushValue(GenInstanceMethodContext cxt);
    }
}
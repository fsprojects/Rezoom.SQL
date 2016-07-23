using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using Rezoom.ADO.Materialization.GenBuilders;
using Rezoom.ADO.Materialization.TypeInfo;

namespace Rezoom.ADO.Materialization
{
    internal static class RowReaderTemplateGenerator
    {
        private static void ImplementRowReader(TypeBuilder builder, Type targetType)
        {
            var profile = TypeProfile.OfType(targetType);
            var gen = profile.Columns.Any(c => c.Setter == null)
                ? new ConstructorGenBuilder(profile.PrimaryConstructor) as IGenBuilder
                : new PropertyAssignmentGenBuilder(targetType);
            builder.AddInterfaceImplementation(typeof(IRowReader<>).MakeGenericType(targetType));

            GenInstanceMethodContext toEntityContext;
            {
                var toEntity = builder.DefineMethod
                    (nameof(IRowReader<object>.ToEntity), MethodAttributes.Public | MethodAttributes.Virtual);
                toEntity.SetParameters();
                toEntity.SetReturnType(targetType);
                var il = toEntity.GetILGenerator();
                var thisLocal = il.DeclareLocal(builder);
                il.Emit(OpCodes.Ldarg_0); // load this
                il.Emit(OpCodes.Stloc, thisLocal);
                toEntityContext = new GenInstanceMethodContext(il, thisLocal);
            }

            GenProcessColumnMapContext columnContext;
            {
                var processColumnMap = builder.DefineMethod
                    (nameof(IRowReader<object>.ProcessColumnMap), MethodAttributes.Public | MethodAttributes.Virtual);
                processColumnMap.SetParameters(typeof(ColumnMap));
                var il = processColumnMap.GetILGenerator();
                var thisLocal = il.DeclareLocal(builder);
                il.Emit(OpCodes.Ldarg_0); // load this
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Stloc, thisLocal);
                columnContext = new GenProcessColumnMapContext(il, thisLocal);
                il.Emit(OpCodes.Ldarg_1); // load column map
                il.Emit(OpCodes.Stloc, columnContext.ColumnMap);
            }

            GenProcessRowContext rowContext;
            {
                var processRow = builder.DefineMethod
                    (nameof(IRowReader<object>.ProcessRow), MethodAttributes.Public | MethodAttributes.Virtual);
                processRow.SetParameters(typeof(object[]));
                var il = processRow.GetILGenerator();
                var thisLocal = il.DeclareLocal(builder);
                il.Emit(OpCodes.Ldarg_0); // load this
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Stloc, thisLocal);
                rowContext = new GenProcessRowContext(il, thisLocal);
                il.Emit(OpCodes.Ldarg_1); // load row
                il.Emit(OpCodes.Stloc, rowContext.Row);
            }

            var consIL = builder.DefineConstructor
                (MethodAttributes.Public, CallingConventions.HasThis, Type.EmptyTypes).GetILGenerator();
            consIL.Emit(OpCodes.Ldarg_0); // load this

            foreach (var prop in gen.Properties)
            {
                prop.InstallFields(builder, consIL);
                prop.InstallProcessingLogic(columnContext);
                prop.InstallPushValue(toEntityContext);
            }
            {
                var marked = false;
                foreach (var prop in gen.Properties.OrderByDescending(p => p.Singular))
                {
                    if (!marked && !prop.Singular)
                    {
                        rowContext.IL.MarkLabel(rowContext.SkipSingularProperties);
                        marked = true; 
                    }
                    prop.InstallProcessingLogic(rowContext);
                }
                if (!marked) rowContext.IL.MarkLabel(rowContext.SkipSingularProperties);
            }

            gen.InstallConstructor(toEntityContext.IL);
            consIL.Emit(OpCodes.Pop); // pop this
            consIL.Emit(OpCodes.Ret);
            columnContext.IL.Emit(OpCodes.Pop); // pop this
            columnContext.IL.Emit(OpCodes.Ret);
            rowContext.IL.Emit(OpCodes.Pop); // pop this
            rowContext.IL.Emit(OpCodes.Ret);
            toEntityContext.IL.Emit(OpCodes.Ret); // return constructed object
        }

        private static void ImplementRowReaderTemplate(TypeBuilder builder, Type targetType, Type readerType)
        {
            var cons = readerType.GetConstructor(Type.EmptyTypes);
            if (cons == null) throw new Exception("No default constructor for reader");
            builder.DefineDefaultConstructor(MethodAttributes.Public);
            builder.AddInterfaceImplementation(typeof(IRowReaderTemplate<>).MakeGenericType(targetType));
            var creator = builder.DefineMethod
                (nameof(IRowReaderTemplate<object>.CreateReader), MethodAttributes.Public | MethodAttributes.Virtual);
            creator.SetParameters();
            creator.SetReturnType(typeof(IRowReader<>).MakeGenericType(targetType));
            var il = creator.GetILGenerator();
            il.Emit(OpCodes.Newobj, cons);
            il.Emit(OpCodes.Ret);
        }

        public static object GenerateReaderTemplate(Type targetType)
        {
            // create a dynamic assembly to house our dynamic type
            var assembly = new AssemblyName($"Readers.{targetType.FullName}");
            var appDomain = System.Threading.Thread.GetDomain();
            var assemblyBuilder = appDomain.DefineDynamicAssembly(assembly, AssemblyBuilderAccess.Run);
            var moduleBuilder = assemblyBuilder.DefineDynamicModule(assembly.Name);

            // create the dynamic IRowReader<T> type
            var reader = moduleBuilder.DefineType
                ( $"{targetType.Name}Reader"
                , TypeAttributes.Public | TypeAttributes.AutoClass | TypeAttributes.AnsiClass
                , typeof(object)
                );
            ImplementRowReader(reader, targetType);

            var template = moduleBuilder.DefineType
                ($"{targetType.Name}ReaderTemplate"
                , TypeAttributes.Public | TypeAttributes.AutoClass | TypeAttributes.AnsiClass
                , typeof(object)
                );
            ImplementRowReaderTemplate(template, targetType, reader.CreateType());
            var templateType = template.CreateType();
            return Activator.CreateInstance(templateType);
        }
    }
}
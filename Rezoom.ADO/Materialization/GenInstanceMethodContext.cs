using System.Reflection.Emit;

namespace Rezoom.ADO.Materialization
{
    internal class GenInstanceMethodContext
    {
        public GenInstanceMethodContext(ILGenerator il, LocalBuilder @this)
        {
            IL = il;
            This = @this;
        }
        public LocalBuilder This { get; }
        public ILGenerator IL { get; }
    }

    internal class GenProcessColumnMapContext : GenInstanceMethodContext
    {
        public GenProcessColumnMapContext(ILGenerator il, LocalBuilder @this) : base(il, @this)
        {
            ColumnMap = il.DeclareLocal(typeof(ColumnMap));
        }
        public LocalBuilder ColumnMap { get; }
    }

    internal class GenProcessRowContext : GenInstanceMethodContext
    {
        public GenProcessRowContext(ILGenerator il, LocalBuilder @this) : base(il, @this)
        {
            SkipSingularProperties = il.DefineLabel();
            Row = il.DeclareLocal(typeof(object[]));
        }
        public LocalBuilder Row { get; }
        /// <summary>
        /// Label to skip to after all "singular" properties.
        /// </summary>
        public Label SkipSingularProperties { get; }
    }
}

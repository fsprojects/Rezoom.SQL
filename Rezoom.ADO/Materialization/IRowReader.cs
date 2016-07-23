namespace Rezoom.ADO.Materialization
{
    public interface IRowReaderTemplate<out T>
    {
        IRowReader<T> CreateReader();
    }
    public interface IRowReader<out T>
    {
        void ProcessColumnMap(ColumnMap map);
        void ProcessRow(object[] row);
        T ToEntity();
    }

    public static class RowReaderTemplate<T>
    {
        public static readonly IRowReaderTemplate<T> Template = (IRowReaderTemplate<T>)
            RowReaderTemplateGenerator.GenerateReaderTemplate(typeof(T));
    }
}
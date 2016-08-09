using System.Data;

namespace Rezoom.ADO
{
    public abstract class ResultSetProcessor
    {
        public abstract void BeginResultSet(IDataReader reader);
        public abstract void ProcessRow();
    }
}

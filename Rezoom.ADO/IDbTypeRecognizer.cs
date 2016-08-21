using System.Data;

namespace Rezoom.ADO
{
    public interface IDbTypeRecognizer
    {
        void GetDbType(ref object value, out DbType dbType);
    }
}
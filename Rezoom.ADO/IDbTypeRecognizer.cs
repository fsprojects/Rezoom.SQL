using System.Data;

namespace Rezoom.ADO
{
    public interface IDbTypeRecognizer
    {
        DbType GetDbType(object value);
    }
}
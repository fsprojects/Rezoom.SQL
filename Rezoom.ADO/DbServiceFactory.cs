using System.Data.Common;
using Rezoom;

namespace Rezoom.ADO
{
    public abstract class DbServiceFactory : ServiceFactory
    {
        protected abstract DbConnection CreateConnection();
        protected virtual IDbTypeRecognizer CreateDbTypeRecognizer() => new DbTypeRecognizer();

        public override LivingService<T> CreateService<T>(ServiceContext context)
        {
            if (typeof(T) == typeof(DbConnection))
            {
                var conn = CreateConnection();
                return new LivingService<T>(ServiceLifetime.ExecutionLocal, (T)(object)conn);
            }
            if (typeof(T) == typeof(IDbTypeRecognizer))
            {
                var recognizer = CreateDbTypeRecognizer();
                return new LivingService<T>(ServiceLifetime.ExecutionLocal, (T)recognizer);
            }
            if (typeof(T) == typeof(CommandBatch))
            {
                var dbConnection = context.GetService<DbConnection>();
                var dbTypeRecognizer = context.GetService<IDbTypeRecognizer>();
                var cmdContext = new CommandBatch(dbConnection, dbTypeRecognizer);
                return new LivingService<T>(ServiceLifetime.StepLocal, (T)(object)cmdContext);
            }
            return null;
        }
    }
}

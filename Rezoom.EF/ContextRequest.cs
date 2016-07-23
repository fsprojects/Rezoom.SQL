using System;
using System.Data.Entity;
using System.Threading.Tasks;

namespace Rezoom.EF
{
    public abstract class ContextErrand<TContext, T> : CS.AsynchronousErrand<T>
        where TContext : DbContext
    {
        public override object DataSource => typeof(TContext);
        public override object SequenceGroup => typeof(TContext);

        protected abstract Func<Task<T>> Prepare(TContext db);

        public sealed override Func<Task<T>> Prepare(ServiceContext context)
        {
            var db = context.GetService<TContext>();
            return Prepare(db);
        }
    }
}
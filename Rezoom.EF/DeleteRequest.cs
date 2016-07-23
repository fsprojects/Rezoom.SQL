using System;
using System.Data.Entity;
using System.Threading.Tasks;

namespace Rezoom.EF
{
    public class DeleteErrand<TContext, TEntity> : ContextErrand<TContext, object>
        where TContext : DbContext
        where TEntity : class
    {
        private readonly TEntity _entity;
        private readonly Func<TContext, IDbSet<TEntity>> _set;

        public DeleteErrand(Func<TContext, IDbSet<TEntity>> set, TEntity entity)
        {
            _set = set;
            _entity = entity;
        }

        public override bool Mutation => true;
        public override bool Idempotent => true;

        protected override Func<Task<object>> Prepare(TContext db)
        {
            var set = _set(db);
            set.Attach(_entity);
            set.Remove(_entity);
            return async () =>
            {
                await db.SaveChangesAsync();
                return null;
            };
        }
    }
}
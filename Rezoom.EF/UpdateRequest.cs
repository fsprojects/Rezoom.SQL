using System;
using System.Data.Entity;
using System.Threading.Tasks;

namespace Rezoom.EF
{
    public class UpdateErrand<TContext, TEntity> : ContextErrand<TContext, object>
        where TContext : DbContext
        where TEntity : class, new()
    {
        private readonly TEntity _entity;
        private readonly Func<TContext, IDbSet<TEntity>> _set;
        private readonly Action<TEntity> _change;

        public UpdateErrand(Func<TContext, IDbSet<TEntity>> set, TEntity entity, Action<TEntity> change)
        {
            _set = set;
            _entity = entity;
            _change = change;
        }

        public override bool Mutation => true;
        public override bool Idempotent => false;

        protected override Func<Task<object>> Prepare(TContext db)
        {
            var copy = Cloner<TEntity>.Instance.Clone(_entity);
            var set = _set(db);
            set.Attach(copy);
            _change(copy);
            return async () =>
            {
                await db.SaveChangesAsync();
                return null;
            };

        }
    }
}
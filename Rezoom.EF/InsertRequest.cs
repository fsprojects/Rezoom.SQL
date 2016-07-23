using System;
using System.Data.Entity;
using System.Threading.Tasks;

namespace Rezoom.EF
{
    public class InsertRequest<TContext, TEntity> : ContextRequest<TContext, TEntity>
        where TContext : DbContext
        where TEntity : class
    {
        private readonly Func<TContext, IDbSet<TEntity>> _set;
        private readonly TEntity _toInsert;

        public InsertRequest(Func<TContext, IDbSet<TEntity>> set, TEntity toInsert)
        {
            _set = set;
            _toInsert = toInsert;
        }

        public override bool Mutation => true;
        public override bool Idempotent => false;

        protected override Func<Task<TEntity>> Prepare(TContext db)
        {
            _set(db).Add(_toInsert);
            return async () =>
            {
                await db.SaveChangesAsync();
                return _toInsert;
            };
        }
    }
}
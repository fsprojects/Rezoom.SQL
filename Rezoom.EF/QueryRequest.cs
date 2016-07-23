using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Threading.Tasks;
using EntityFramework.Extensions;

namespace Rezoom.EF
{
    public class QueryErrand<TContext, T> : ContextErrand<TContext, List<T>>
        where TContext : DbContext
        where T : class
    {
        private readonly Func<TContext, IQueryable<T>> _query;

        public QueryErrand(Func<TContext, IQueryable<T>> query)
        {
            _query = query;
        }

        public override bool Mutation => false;
        public override bool Idempotent => true;

        protected override Func<Task<List<T>>> Prepare(TContext db)
        {
            var future = _query(db).AsNoTracking().Future();
            return () => Task.FromResult(future.ToList()); // unfortunately, futures don't support ToListAsync
        }
    }
}

using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Text;

namespace Rezoom.ADO
{
    internal class MultiCommandBuilder
    {
        /// <summary>
        /// We put this after each command to terminate it.
        /// The extra characters are intended to guard against accidental issues like
        /// unclosed string literals or block comments spilling into the next command.
        /// </summary>
        private const string CommandTerminator = ";--'*/;";

        private readonly IDbTypeRecognizer _recognizer;
        private readonly StringBuilder _sql = new StringBuilder();
        private readonly Dictionary<string, int> _locals = new Dictionary<string, int>();
        private readonly List<string> _separators = new List<string>();
        private readonly List<MultiCommandParameter> _parameters = new List<MultiCommandParameter>();

        public MultiCommandBuilder(IDbTypeRecognizer recognizer)
        {
            _recognizer = recognizer;
        }

        private string GetParameter(object arg)
        {
            var name = $"@RZPARAM{_parameters.Count}";
            DbType dbType;
            _recognizer.GetDbType(ref arg, out dbType);
            var parameter = new MultiCommandParameter(name, dbType, arg);
            _parameters.Add(parameter);
            return name;
        }

        private string ToFormat(Dictionary<Local, string> locals, object arg)
        {
            var local = arg as Local;
            if (local != null)
            {
                string name;
                if (locals.TryGetValue(local, out name)) return name;
                int count;
                if (!_locals.TryGetValue(local.Name, out count))
                {
                    count = 0;
                }
                _locals[local.Name] = count + 1;
                name = $"{local.Name}_{count:D}";
                locals[local] = name;
                return name;
            }
            var enumerable = arg is string ? null : arg as IEnumerable;
            if (enumerable != null)
            {
                var subArgs = new List<string>();
                foreach (var subArg in enumerable)
                {
                    subArgs.Add(ToFormat(locals, subArg));
                }
                return "(" + string.Join(",", subArgs) + ")";
            }
            return GetParameter(arg);
        }

        public void AppendCommand(FormattableString command)
        {
            if (_sql.Length > 0)
            {
                var sep = $"RZSEP_{Guid.NewGuid():N}";
                _separators.Add(sep);
                _sql.AppendLine(CommandTerminator);
                _sql.AppendLine($"SELECT NULL AS {sep}");
                _sql.AppendLine(CommandTerminator);
            }
            var locals = new Dictionary<Local, string>();
            var argValues = command.GetArguments();
            var formatArgs = new object[command.ArgumentCount];
            for (var i = 0; i < argValues.Length; i++)
            {
                formatArgs[i] = ToFormat(locals, argValues[i]);
            }
            _sql.AppendLine(string.Format(command.Format, formatArgs));
        }

        public DbCommand CreateCommand(DbConnection connection)
        {
            var cmd = connection.CreateCommand();
            cmd.CommandText = _sql.ToString();
            foreach (var par in _parameters)
            {
                var dbPar = cmd.CreateParameter();
                dbPar.ParameterName = par.Name;
                dbPar.DbType = par.Type;
                dbPar.Value = par.Value;
                cmd.Parameters.Add(dbPar);
            }
            cmd.Connection = connection;
            return cmd;
        }

        public IReadOnlyList<string> Separators => _separators;
    }
}

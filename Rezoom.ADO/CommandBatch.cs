using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Rezoom.ADO
{
    /// <summary>
    /// Builds up a batch of SQL strings to run in a single IDbCommand.
    /// </summary>
    internal class CommandBatch : IDisposable
    {
        /// <summary>
        /// We put this after each command to terminate it.
        /// The extra characters are intended to guard against accidental issues like
        /// unclosed string literals or block comments spilling into the next command.
        /// </summary>
        private const string CommandTerminator = ";--'*/;";

        private readonly IDbTypeRecognizer _typeRecognizer;
        private readonly DbCommand _command;
        private readonly List<Command> _commands = new List<Command>();
        private readonly List<string> _sql = new List<string>();

        private Task<ResultSetProcessor[]> _executing;

        public CommandBatch(DbConnection connection, IDbTypeRecognizer typeRecognizer)
        {
            _typeRecognizer = typeRecognizer;
            _command = connection.CreateCommand();
            _command.Connection = connection;
        }

        public Func<Task<T>> Prepare<T>(Command<T> command)
        {
            if (_executing != null)
                throw new InvalidOperationException("Command is already executing");
            var parameterValues = command.Text.GetArguments();
            var parameterNames = new object[parameterValues.Length];
            for (var i = 0; i < parameterValues.Length; i++)
            {
                var dbParamName = $"@DRBATCHPARAM_{_command.Parameters.Count}";
                var dbParam = _command.CreateParameter();
                dbParam.ParameterName = dbParamName;
                dbParam.Value = parameterValues[i];
                dbParam.DbType = _typeRecognizer.GetDbType(parameterValues[i]);
                _command.Parameters.Add(dbParam);
                parameterNames[i] = dbParamName;
            }
            var sqlReferencingParams = string.Format(command.Text.Format, parameterNames);

            var commandIndex = _commands.Count;
            _commands.Add(command);
            _sql.Add(sqlReferencingParams);
            return async () =>
            {
                var proc = await GetResultSet(commandIndex);
                return command.ExtractResult(proc);
            };
        }

        private async Task<ResultSetProcessor[]> GetAllResultSets()
        {
            var separators = new List<string>();
            var gluedText = new StringBuilder();
            foreach (var sql in _sql)
            {
                if (gluedText.Length > 0)
                {
                    var sep = $"DRSEP_{Guid.NewGuid():N}";
                    gluedText.AppendLine(CommandTerminator);
                    gluedText.AppendLine($"SELECT NULL as {sep};");
                    separators.Add(sep);
                }
                gluedText.AppendLine(sql);
            }
            _command.CommandText = gluedText.ToString();
            using (var reader = await _command.ExecuteReaderAsync().ConfigureAwait(false))
            {
                var index = 0;
                var allResults = new ResultSetProcessor[_commands.Count];
                for (var i = 0; i < allResults.Length; i++)
                {
                    allResults[i] = _commands[i].Processor();
                }
                do
                {
                    var proc = allResults[index];
                    // If we hit our separator, that's the end of a command's result sets.
                    if (separators.Count > index && reader.FieldCount == 1 && reader.GetName(0) == separators[index])
                    {
                        index++;
                    }
                    else
                    {
                        proc.BeginResultSet(reader);
                        while (await reader.ReadAsync().ConfigureAwait(false))
                        {
                            proc.ProcessRow();
                        }
                    }
                } while (await reader.NextResultAsync().ConfigureAwait(false));
                if (index + 1 < allResults.Length)
                {
                    throw new InvalidDataException
                        ("Missing separator from result sets. This may be caused by invalid SQL syntax.");
                }
                return allResults;
            }
        }

        private async Task<ResultSetProcessor> GetResultSet(int index)
        {
            if (_executing != null)
            {
                var allResults = await _executing.ConfigureAwait(false);
                return allResults[index];
            }
            _executing = GetAllResultSets();
            var results = await _executing.ConfigureAwait(false);
            return results[index];
        }

        public void Dispose() => _command.Dispose();
    }
}
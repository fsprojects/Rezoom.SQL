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
        private readonly List<string> _commands = new List<string>();

        private Task<List<List<CommandResponse>>> _executing;

        public CommandBatch(DbConnection connection, IDbTypeRecognizer typeRecognizer)
        {
            _typeRecognizer = typeRecognizer;
            _command = connection.CreateCommand();
            _command.Connection = connection;
        }

        public Func<Task<IReadOnlyList<CommandResponse>>> Prepare(Command command)
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
            _commands.Add(sqlReferencingParams);
            return () => GetResultSet(commandIndex);
        }

        private async Task<List<List<CommandResponse>>> GetAllResultSets()
        {
            var separators = new List<string>();
            var gluedText = new StringBuilder();
            foreach (var command in _commands)
            {
                if (gluedText.Length > 0)
                {
                    var sep = $"DRSEP_{Guid.NewGuid():N}";
                    gluedText.AppendLine(CommandTerminator);
                    gluedText.AppendLine($"SELECT NULL as {sep};");
                    separators.Add(sep);
                }
                gluedText.AppendLine(command);
            }
            _command.CommandText = gluedText.ToString();
            using (var reader = await _command.ExecuteReaderAsync())
            {
                var sepi = 0;
                var allResults = new List<List<CommandResponse>>();
                var currentCommandResults = new List<CommandResponse>();
                do
                {
                    var fieldNames = Enumerable.Range(0, reader.FieldCount)
                        .Select(reader.GetName)
                        .ToArray();
                    // If we hit our separator, that's the end of a command's result sets.
                    if (separators.Count > sepi && fieldNames.Length == 1 && fieldNames[0] == separators[sepi])
                    {
                        allResults.Add(currentCommandResults);
                        currentCommandResults = new List<CommandResponse>();
                        sepi++;
                    }
                    else
                    {
                        var rows = new List<IReadOnlyList<object>>();
                        while (await reader.ReadAsync())
                        {
                            var row = new object[reader.FieldCount];
                            reader.GetValues(row);
                            rows.Add(row);
                        }
                        currentCommandResults.Add(new CommandResponse(fieldNames, rows));
                    }
                } while (await reader.NextResultAsync());
                if (sepi < allResults.Count)
                {
                    throw new InvalidDataException($"Unexpected result sets missing separator");
                }
                // The last result set doesn't have a trailing separator, so we need to add it on here.
                allResults.Add(currentCommandResults);
                return allResults;
            }
        }

        private async Task<IReadOnlyList<CommandResponse>> GetResultSet(int index)
        {
            if (_executing != null)
            {
                var allResults = await _executing;
                return allResults[index];
            }
            _executing = GetAllResultSets();
            var results = await _executing;
            return results[index];
        }

        public void Dispose() => _command.Dispose();
    }
}
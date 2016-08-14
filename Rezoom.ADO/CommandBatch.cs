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
        private readonly MultiCommandBuilder _builder;
        private readonly DbConnection _connection;
        private readonly List<Command> _commands = new List<Command>();
        private DbCommand _command;

        private Task<ResultSetProcessor[]> _executing;

        public CommandBatch(DbConnection connection, IDbTypeRecognizer typeRecognizer)
        {
            _connection = connection;
            _builder = new MultiCommandBuilder(typeRecognizer);
        }

        public Func<Task<T>> Prepare<T>(Command<T> command)
        {
            if (_executing != null)
                throw new InvalidOperationException("Command is already executing");
            var commandIndex = _commands.Count;
            _builder.AppendCommand(command.Text);
            _commands.Add(command);
            return async () =>
            {
                var proc = await GetResultSet(commandIndex);
                return command.ExtractResult(proc);
            };
        }

        private async Task<ResultSetProcessor[]> GetAllResultSets()
        {
            var separators = _builder.Separators;
            _command = _builder.CreateCommand(_connection);
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

        public void Dispose() => _command?.Dispose();
    }
}
using System;

namespace Rezoom.ADO
{
    /// <summary>
    /// This gives you a local name for something like a temporary table; that will work in the presence of batching.
    /// </summary>
    public sealed class Local : IEquatable<Local>
    {
        public Local(string name)
        {
            Name = name;
        }

        public string Name { get; }

        public bool Equals(Local other) => other != null && other.Name == Name;

        public override string ToString() => Name;
        public override bool Equals(object obj) => Equals(obj as Local);
        public override int GetHashCode() => Name.GetHashCode();
    }
}
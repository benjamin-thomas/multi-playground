using System;
using System.Collections.Generic;

namespace NpgsqlEfCoreDemo
{
    public partial class Customer
    {
        public int Id { get; set; }
        public string Name { get; set; } = null!;
        public string? AlternativeName { get; set; }
    }
}

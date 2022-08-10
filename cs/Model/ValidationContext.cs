namespace Model;

internal abstract class ValidationContext
{
    required internal Application App { get; init; }

    internal HashSet<Expression> Expressions { get; } = new();

    internal abstract int Count { get; }
}

internal sealed class ValidationContext<ArgsNumber> : ValidationContext where ArgsNumber : CallArgs
{
    internal override int Count => ArgsNumber.Count;
}
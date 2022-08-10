namespace Model;

/// <summary>Cette expression représente une addition</summary>
public sealed class AdditionExpression : Expression
{
    /// <summary>Initializes a new instance of the <see cref="AdditionExpression"/> class.</summary>
    /// <param name="first">Le premier membre de l'addition</param>
    /// <param name="second">Le second membre de l'addition</param>
    public AdditionExpression(Expression first, Expression second)
    {
        First = first;
        Second = second;
    }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
        => sb.ExtractElem(First, numIndex).Append(" + ").ExtractElem(Second, numIndex);

    /// <summary>Le premier membre de l'addition</summary>
    public Expression First { get; set; }

    /// <summary>Le second membre de l'addition</summary>
    public Expression Second { get; set; }

    internal override bool Validate(ValidationContext vc) => vc.Expressions.Contains(First) && vc.Expressions.Contains(Second);
}
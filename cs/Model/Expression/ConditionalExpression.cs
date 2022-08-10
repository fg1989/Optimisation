namespace Model;

/// <summary>Cette expression représente une code executé conditionnellemnt</summary>
public class ConditionalExpression : Expression
{
    /// <summary>Initializes a new instance of the <see cref="ConditionalExpression"/> class.</summary>
    /// <param name="cond">Cette expression est la condition, elle doit avoir été évaluée précédement</param>
    /// <param name="zeroBranch">Cette expression sera executée si la condition vaut 0, elle ne doit pas avoir été évaluée précédement</param>
    /// <param name="nonZeorBranc">Cette expression sera executée si la condition ne vaut pas 0, elle ne doit pas avoir été évaluée précédement</param>
    public ConditionalExpression(Expression cond, Expression zeroBranch, Expression nonZeorBranc)
    {
        Cond = cond;
        ZeroBranch = zeroBranch;
        NonZeroBranc = nonZeorBranc;
    }

    /// <summary>Cette expression est la condition, elle doit avoir été évaluée précédement</summary>
    public Expression Cond { get; set; }

    /// <summary>Cette expression sera executée si la condition vaut 0, elle ne doit pas avoir été évaluée précédement</summary>
    public Expression ZeroBranch { get; set; }

    /// <summary>Cette expression sera executée si la condition ne vaut pas 0, elle ne doit pas avoir été évaluée précédement</summary>
    public Expression NonZeroBranc { get; set; }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
    {
        sb.ExtractElem(Cond, numIndex)
            .Append(" ? [");

        if (numIndex.Discard(NonZeroBranc))
            sb.AppendElem(NonZeroBranc, numIndex);
        else
            sb.Error().Append("#invalidDoubleEvaluation").Normal();

        sb.Append("] : [");

        if (numIndex.Discard(ZeroBranch))
            sb.AppendElem(ZeroBranch, numIndex);
        else
            sb.Error().Append("#invalidDoubleEvaluation").Normal();

        sb.Append(']');
    }

    internal override bool Validate(ValidationContext vc)
    {
        if (NonZeroBranc == ZeroBranch || vc.Expressions.Contains(NonZeroBranc) || vc.Expressions.Contains(ZeroBranch))
            return false;

        vc.Expressions.Add(NonZeroBranc);
        vc.Expressions.Add(ZeroBranch);
        return vc.Expressions.Contains(Cond);
    }
}
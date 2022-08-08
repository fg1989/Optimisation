namespace Model;

internal abstract class FuncDictIndex
{
    internal abstract void GetText(ColoredStringBuilder sb, Fonction expr);
}

internal sealed class RealFuncDictIndex : FuncDictIndex
{
    internal ColoredStringBuilder RegisterElem(ColoredStringBuilder sb, Fonction expr)
    {
        if (exprs.ContainsKey(expr))
            return sb.Error().Append("invalidDoubleEvaluation").Normal();

        if (reservations.Remove(expr, out int index))
        {
            exprs[expr] = index;
            sb.Append('$').Append(index);
        }
        else
        {
            int tmp = exprs.Count + reservations.Count;
            exprs[expr] = tmp;
            sb.Append('$').Append(tmp);
        }

        return sb;
    }

    internal override void GetText(ColoredStringBuilder sb, Fonction expr)
    {
        if (exprs.TryGetValue(expr, out int value))
        {
            sb.Append('$').Append(value);
        }
        else
        {
            int index = exprs.Count + reservations.Count;
            reservations[expr] = index;
            sb.Append('$').Append(index);
        }
    }

    internal IEnumerable<int> Check() => reservations.Values;

    private readonly Dictionary<Fonction, int> exprs = new();
    private readonly Dictionary<Fonction, int> reservations = new();
}

internal sealed class EmptyFuncDictIndex : FuncDictIndex
{
    internal override void GetText(ColoredStringBuilder sb, Fonction expr) => sb.Append("call");
}

internal abstract class VarDictIndex : FuncDictIndex
{
    private protected VarDictIndex(FuncDictIndex dictIndex)
    {
        this.dictIndex = dictIndex;
    }

    internal abstract bool Discard(Expression expr);

    internal override void GetText(ColoredStringBuilder sb, Fonction expr) => dictIndex.GetText(sb, expr);

    internal abstract void GetText(ColoredStringBuilder sb, Expression expr);

    private protected void ConvertHelper(ColoredStringBuilder sb, Expression expr) => expr.ToString(sb, this);

    private readonly FuncDictIndex dictIndex;
}

internal sealed class RealDictIndex : VarDictIndex
{
    public RealDictIndex(FuncDictIndex dictIndex) : base(dictIndex)
    {
    }

    internal ColoredStringBuilder RegisterElem(ColoredStringBuilder sb, Expression expr)
    {
        if (exprs.ContainsKey(expr))
            return sb.Error().Append("#invalidDoubleEvaluation").Normal();

        exprs[expr] = exprs.Count;
        return sb.Append('#').Append(exprs.Count - 1);
    }

    internal override void GetText(ColoredStringBuilder sb, Expression expr)
    {
        if (exprs.TryGetValue(expr, out int value))
        {
            if (value == -1)
                sb.Error().Append("#invalidDoubleEvaluation").Normal();
            else
                sb.Append('#').Append(value);
        }
        else
        {
            sb.Error().Append("[invalid](");
            ConvertHelper(sb, expr);
            sb.Append(')').Normal();
        }
    }

    private readonly Dictionary<Expression, int> exprs = new();

    internal override bool Discard(Expression expr)
    {
        if (exprs.ContainsKey(expr))
            return false;

        exprs[expr] = -1;
        return true;
    }
}

internal sealed class EmptyDictIndex : VarDictIndex
{
    public EmptyDictIndex() : base(new EmptyFuncDictIndex())
    {
    }

    internal override void GetText(ColoredStringBuilder sb, Expression expr)
    {
        sb.Append('(');
        ConvertHelper(sb, expr);
        sb.Append(')');
    }

    internal override bool Discard(Expression expr) => true;
}
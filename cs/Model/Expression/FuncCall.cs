namespace Model;

/// <summary>Cette expression représente un appel de fonction</summary>
public abstract class FuncCall : Expression
{
    private protected FuncCall(Fonction func, Expression[] expressions)
    {
        Func = func;
        Param = expressions;
    }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
    {
        sb.ExtractElem(Func, numIndex).Append('(');

        IEnumerator<Expression> enu = ((IEnumerable<Expression>)Param).GetEnumerator();

        if (enu.MoveNext())
            sb.ExtractElem(enu.Current, numIndex);

        while (enu.MoveNext())
            sb.Append(", ").ExtractElem(enu.Current, numIndex);

        sb.Append(')');
    }

    /// <summary>La fonction qui est appellée</summary>
    public Fonction Func { get; set; }

    /// <summary>Les paramètres de la fonction</summary>
    public Expression[] Param { get; }

    /// <summary>Cette méthode sert de constructeur</summary>
    /// <param name="func">La fonction qui est appellée</param>
    public static FuncCall<Args0> Create(Fonction<Args0> func) => new(func, Array.Empty<Expression>());

    /// <summary>Cette méthode sert de constructeur</summary>
    /// <param name="func">La fonction qui est appellée</param>
    /// <param name="expr1">Le premier paramètre de la fonction</param>
    public static FuncCall<Args1> Create(Fonction<Args1> func, Expression expr1) => new(func, new Expression[] { expr1 });

    /// <summary>Cette méthode sert de constructeur</summary>
    /// <param name="func">La fonction qui est appellée</param>
    /// <param name="expr1">Le premier paramètre de la fonction</param>
    /// <param name="expr2">Le deuxième paramètre de la fonction</param>
    public static FuncCall<Args2> Create(Fonction<Args2> func, Expression expr1, Expression expr2)
        => new(func, new Expression[] { expr1, expr2 });

    /// <summary>Cette méthode sert de constructeur</summary>
    /// <param name="func">La fonction qui est appellée</param>
    /// <param name="expr1">Le premier paramètre de la fonction</param>
    /// <param name="expr2">Le deuxième paramètre de la fonction</param>
    /// <param name="expr3">Le troisième paramètre de la fonction</param>
    public static FuncCall<Args3> Create(Fonction<Args3> func, Expression expr1, Expression expr2, Expression expr3)
        => new(func, new Expression[] { expr1, expr2, expr3 });
}

/// <summary>Cette expression représente un appel de fonction</summary>
/// <typeparam name="ArgsNumber">Le nombre d'arguments de la fonction</typeparam>
public sealed class FuncCall<ArgsNumber> : FuncCall where ArgsNumber : CallArgs
{
    internal FuncCall(Fonction<ArgsNumber> func, Expression[] args) : base(func, args)
    {
    }
}
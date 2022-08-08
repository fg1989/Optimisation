namespace Model;

/// <summary>Cette classe représente une fonction</summary>
public abstract class Fonction : CompilationSource
{
    private protected Fonction(List<Expression> actions)
    {
        Actions = actions;
    }

    /// <summary>La liste des toutes les actions qui sont effectuées par la fonction</summary>
    /// <remarks>La dernière action est la valeur de retour</remarks>
    public List<Expression> Actions { get; }

    internal override void ToString(ColoredStringBuilder sb, int decalage) => ToString(sb, decalage, new EmptyFuncDictIndex());

    internal ColoredStringBuilder ToString(ColoredStringBuilder sb, int decalage, FuncDictIndex fdi)
    {
        RealDictIndex numIndex = new(fdi);

        foreach (Expression item in Actions)
            sb.AppendTabs(decalage).RegisterElem(item, numIndex).Append(" = ").AppendElem(item, numIndex).AppendLine();

        sb.AppendTabs(decalage).Append("return ").ExtractElem(Actions[^1], numIndex).AppendLine();

        return sb;
    }
}

/// <summary>Cette classe représente une fonction</summary>
/// <typeparam name="ArgsNumber">Le nombre d'arguments de la fonction</typeparam>
[SuppressMessage(
    "Major Code Smell",
    "S2326:Unused type parameters should be removed",
    Justification = "Le paramètre de type sert uniquement de contrainte, il n'est donc pas utilisé")]
public sealed class Fonction<ArgsNumber> : Fonction where ArgsNumber : CallArgs
{
    /// <summary>Initializes a new instance of the <see cref="Fonction{ArgsNumber}"/> class.</summary>
    /// <param name="first">La première action effectuée par la fonction (chaque fonction doit effectuer au moins une action)</param>
    /// <param name="actions">La liste des toutes le autres actions qui sont effectuées par la fonction</param>
    public Fonction(Expression first, params Expression[] actions) : base(Init(first, actions))
    {
    }

    private static List<Expression> Init(Expression first, Expression[] actions)
    {
        List<Expression> result = new(actions);
        result.Insert(0, first);
        return result;
    }
}
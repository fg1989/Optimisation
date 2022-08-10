namespace Model;

/// <summary>Cette expression représente un calcul qui ne peut pas être optimisé</summary>
public abstract class ExternalExpression : Expression
{
    private protected ExternalExpression(int valeur)
    {
        Valeur = valeur;
    }

    /// <summary>L'index du calcul</summary>
    /// <remarks>Cette valeur sert a distinguer deux calculs différents</remarks>
    public int Valeur { get; set; }

    internal sealed override bool Validate(ValidationContext vc) => true;
}

/// <summary>Cette expression représente un calcul simple qui ne peut pas être optimisé</summary>
public sealed class SimpleExternal : ExternalExpression
{
    /// <summary>Initializes a new instance of the <see cref="SimpleExternal"/> class.</summary>
    /// <param name="valeur">L'index du calcul</param>
    public SimpleExternal(int valeur) : base(valeur)
    {
    }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
        => sb.Append("external[").Append(Valeur).Append(']');
}

/// <summary>Cette expression représente un calcul qui ne peut pas être optimisé et qui ne doit être effectué qu'une seule fois</summary>
public sealed class UniqueExternal : ExternalExpression
{
    /// <summary>Initializes a new instance of the <see cref="UniqueExternal"/> class.</summary>
    /// <param name="valeur">L'index du calcul</param>
    public UniqueExternal(int valeur) : base(valeur)
    {
    }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
        => sb.Append("uniqueExternal[").Append(Valeur).Append(']');
}

/// <summary>Cette expression représente un calcul qui ne peut pas être optimisé et qui ne doit être effectué qu'une seule fois et dont
/// l'ordre avec les autre <see cref="OrderedExternal"/> ne doit pas être modifié (une lecture depuis la console par exemple)</summary>
public sealed class OrderedExternal : ExternalExpression
{
    /// <summary>Initializes a new instance of the <see cref="OrderedExternal"/> class.</summary>
    /// <param name="valeur">L'index du calcul</param>
    public OrderedExternal(int valeur) : base(valeur)
    {
    }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex)
        => sb.Append("orderedExternal[").Append(Valeur).Append(']');
}
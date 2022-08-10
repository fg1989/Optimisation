namespace Model;

/// <summary>Cette expression représente une constante</summary>
public sealed class ConstExpression : Expression
{
    /// <summary>Initializes a new instance of the <see cref="ConstExpression"/> class.</summary>
    /// <param name="valeur">La valeur de la constante</param>
    public ConstExpression(int valeur)
    {
        Valeur = valeur;
    }

    /// <summary>La valeur de la constante</summary>
    public int Valeur { get; set; }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex) => sb.Append(Valeur);

    internal override bool Validate(ValidationContext vc) => true;
}
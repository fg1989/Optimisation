namespace Model;

/// <summary>Cette expression représente la lecture d'un paramètre de la fonction</summary>
public class ParamExpression : Expression
{
    /// <summary>Initializes a new instance of the <see cref="ParamExpression"/> class.</summary>
    /// <param name="valeur">L'indice du paramètre qui doit être lu</param>
    public ParamExpression(int valeur)
    {
        Valeur = valeur;
    }

    /// <summary>L'indice du paramètre qui doit être lu (commence a zéro)</summary>
    /// <remarks>Les paramètres sont indexés a partir de 0</remarks>
    public int Valeur { get; set; }

    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex) => sb.Append('*').Append(Valeur);

    internal override bool Validate(ValidationContext vc) => Valeur < vc.Count;
}
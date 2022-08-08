namespace Model;

/// <summary>Cette classe représente un expression invalide</summary>
/// <remarks>Elle ne doit pas être présente dans le programme final mais peut servir de valeur temporaire pour initialiser une valeur lors des calculs</remarks>
public class InvalidExpression : Expression
{
    internal override void ToString(ColoredStringBuilder sb, VarDictIndex numIndex) => sb.Error().Append("invalidExpression").Normal();
}
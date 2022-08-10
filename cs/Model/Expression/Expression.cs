namespace Model;

/// <summary>Cette classe représente une expression, un calcul simple qui retourne une valeur</summary>
public abstract class Expression : CompilationBase
{
    internal override sealed void ToString(ColoredStringBuilder sb) => ToString(sb, new EmptyDictIndex());

    internal abstract void ToString(ColoredStringBuilder sb, VarDictIndex numIndex);

    internal abstract bool Validate(ValidationContext vc);
}
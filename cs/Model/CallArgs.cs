namespace Model;

/// <summary>Cette classe modélise une contrainte sur le nombre d'arguments d'une fonction</summary>
public interface CallArgs
{
    internal static abstract int Count { get; }
}

/// <summary>Cette classe représente les fonctions sans arguments</summary>
[SuppressMessage(
    "Major Bug",
    "S3453:Classes should not have only \"private\" constructors",
    Justification = "Les classes statiques ne peuvent pas implémenter d'interface")]
[SuppressMessage("CodeQuality", "IDE0079:Remove unnecessary suppression", Justification = "Bug du système d'erreur")]
public sealed class Args0 : CallArgs
{
    private Args0()
    {
    }

    static int CallArgs.Count => 0;
}

/// <summary>Cette classe représente les fonctions avec 1 argument</summary>
[SuppressMessage(
    "Major Bug",
    "S3453:Classes should not have only \"private\" constructors",
    Justification = "Les classes statiques ne peuvent pas implémenter d'interface")]
[SuppressMessage("CodeQuality", "IDE0079:Remove unnecessary suppression", Justification = "Bug du système d'erreur")]
public sealed class Args1 : CallArgs
{
    private Args1()
    {
    }

    static int CallArgs.Count => 1;
}

/// <summary>Cette classe représente les fonctions avec 2 arguments</summary>
[SuppressMessage(
    "Major Bug",
    "S3453:Classes should not have only \"private\" constructors",
    Justification = "Les classes statiques ne peuvent pas implémenter d'interface")]
[SuppressMessage("CodeQuality", "IDE0079:Remove unnecessary suppression", Justification = "Bug du système d'erreur")]
public sealed class Args2 : CallArgs
{
    private Args2()
    {
    }

    static int CallArgs.Count => 2;
}

/// <summary>Cette classe représente les fonctions avec 3 arguments</summary>
[SuppressMessage(
    "Major Bug",
    "S3453:Classes should not have only \"private\" constructors",
    Justification = "Les classes statiques ne peuvent pas implémenter d'interface")]
[SuppressMessage("CodeQuality", "IDE0079:Remove unnecessary suppression", Justification = "Bug du système d'erreur")]
public sealed class Args3 : CallArgs
{
    private Args3()
    {
    }

    static int CallArgs.Count => 3;
}
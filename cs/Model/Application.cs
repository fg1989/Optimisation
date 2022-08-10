using System.Linq;

namespace Model;

/// <summary>Cette classe représente un programme</summary>
public sealed class Application : CompilationSource
{
    /// <summary>Initializes a new instance of the <see cref="Application"/> class.</summary>
    /// <param name="main">La fonction principale du programme</param>
    /// <param name="funcs">L'ensemble des autres fonctions du programme</param>
    public Application(Fonction main, params Fonction[] funcs)
    {
        Main = main;
        Fonctions = new List<Fonction>(funcs);
    }

    /// <summary>La fonction principale du programme</summary>
    public Fonction Main { get; set; }

    /// <summary>L'ensemble des autres fonctions du programme</summary>
    public List<Fonction> Fonctions { get; }

    internal override void ToString(ColoredStringBuilder sb, int decalage)
    {
        RealFuncDictIndex rfdi = new();

        sb.AppendTabs(decalage).AppendLine("Application :")
            .AppendTabs(decalage + 1)
            .AppendLine("Main :")
            .AppendElem(Main, decalage + 2, rfdi);

        foreach (Fonction item in Fonctions)
            sb.AppendTabs(decalage + 1).RegisterElem(item, rfdi).AppendLine(" :").AppendElem(item, decalage + 2, rfdi).AppendLine();

        IEnumerable<int> err = rfdi.Check();

        if (!err.Any())
            return;

        sb.Error().AppendLine().AppendTabs(decalage + 1).Append("Missing func : ")
            .Append(err.Select(item => item.ToString()).Aggregate((item1, item2) => item1 + ", " + item2))
            .AppendLine()
            .Normal();
    }

    /// <summary>Vérifie si une application est valide</summary>
    public bool Validate()
    {
        foreach (Fonction item in Fonctions)
        {
            if (!item.Validate(this))
                return false;
        }
        return Main.Validate(this);
    }
}
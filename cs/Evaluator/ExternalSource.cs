namespace Evaluator;

/// <summary>Représente une source de valeur depuis laquelle on peut lire des external</summary>
public abstract class ExternalSource
{
    internal abstract int GetValue(int index);
}

/// <summary>Cette source de valeur lit les valeurs depuis la console</summary>
public sealed class ConsoleSource : ExternalSource
{
    internal override int GetValue(int index)
    {
        while (true)
        {
            Console.Write("Value : ");
            if (int.TryParse(Console.ReadLine(), out int res))
                return res;

            Console.WriteLine("Invalid");
        }
    }
}

/// <summary>Cette source de valeur retourne toujours la même valeur</summary>
public sealed class ConstantSource : ExternalSource
{
    /// <summary>Initializes a new instance of the <see cref="ConstantSource"/> class.</summary>
    /// <param name="cons">La valeur qui sera toujours retournée</param>
    public ConstantSource(int cons)
    {
        this.cons = cons;
    }

    internal override int GetValue(int index) => cons;

    private readonly int cons;
}

/// <summary>Cette source de valeur lit les valeurs depuis un dictionaire</summary>
public sealed class DictionarySource : ExternalSource
{
    /// <summary>Initializes a new instance of the <see cref="DictionarySource"/> class.</summary>
    /// <param name="dict">Le dictionaire depuis lequel les valeurs sont lues</param>
    public DictionarySource(Dictionary<int, int> dict)
    {
        this.dict = dict;
    }

    internal override int GetValue(int index) => dict[index];

    private readonly Dictionary<int, int> dict;
}
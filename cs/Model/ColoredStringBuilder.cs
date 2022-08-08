using System.Linq;
using System.Text;

namespace Model;

internal class ColoredStringBuilder
{
    internal ColoredStringBuilder Append(char c)
    {
        current.Append(c);
        return this;
    }

    internal ColoredStringBuilder Append(string v)
    {
        current.Append(v);
        return this;
    }

    internal ColoredStringBuilder Append(int i)
    {
        current.Append(i);
        return this;
    }

    internal ColoredStringBuilder AppendLine(string v)
    {
        current.AppendLine(v);
        return this;
    }

    internal ColoredStringBuilder AppendLine()
    {
        current.AppendLine();
        return this;
    }

    internal ColoredStringBuilder Error()
    {
        error++;
        SwitchColor(ConsoleColor.Red);
        return this;
    }

    internal ColoredStringBuilder Normal()
    {
#if DEBUG
        if (error <= 0)
            throw new Exception();
#endif
        error--;
        if (error == 0)
            SwitchColor(ConsoleColor.White);

        return this;
    }

    private void SwitchColor(ConsoleColor color)
    {
        if (this.color == color)
            return;

        AddCurrent();
        this.color = color;
    }

#if DEBUG
    [SuppressMessage("Blocker Code Smell", "S3877:Exceptions should not be thrown from unexpected methods", Justification = "")]
#endif
    public override string ToString()
    {
        AddCurrent();

#if DEBUG
        if (color != ConsoleColor.White)
            throw new Exception();
#endif

        return text.Select(item => item.Item1).Aggregate((item1, item2) => item1 + item2);
    }

    internal void Print()
    {
        AddCurrent();

#if DEBUG
        if (color != ConsoleColor.White)
            throw new Exception();
#endif

        ConsoleColor save = Console.ForegroundColor;

        foreach ((string, ConsoleColor) item in text)
        {
            Console.ForegroundColor = item.Item2;
            Console.Write(item.Item1);
        }

        Console.ForegroundColor = save;
    }

    private void AddCurrent()
    {
        if (current.Length <= 0)
            return;

        text.Add((current.ToString(), color));
        current = new();
    }

    private StringBuilder current = new();
    private ConsoleColor color = ConsoleColor.White;
    private int error;

    private readonly List<(string, ConsoleColor)> text = new();
}
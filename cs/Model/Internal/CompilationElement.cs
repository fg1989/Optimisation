global using System;
global using System.Collections.Generic;
global using System.Diagnostics.CodeAnalysis;

namespace Model;

/// <summary>Cette classe est un helper pour tout les éléments qui font partie de la compilation</summary>
public abstract class CompilationBase
{
    /// <inheritdoc/>
    public sealed override string ToString()
    {
        ColoredStringBuilder sb = new();
        ToString(sb);
        return sb.ToString();
    }

    /// <summary>Affiche l'element dans la console</summary>
    public void Print()
    {
        ColoredStringBuilder sb = new();
        ToString(sb);
        sb.Print();
    }

    internal abstract void ToString(ColoredStringBuilder sb);
}

/// <summary>Cette classe est un helper pour tout les éléments qui font partie de la compilation</summary>
public abstract class CompilationSource : CompilationBase
{
    internal sealed override void ToString(ColoredStringBuilder sb) => ToString(sb, 0);

    internal abstract void ToString(ColoredStringBuilder sb, int decalage);
}

internal static class CompilationExtension
{
    internal static ColoredStringBuilder AppendElem(this ColoredStringBuilder sb, Fonction ce, int decalage, FuncDictIndex fdi)
        => ce.ToString(sb, decalage, fdi);

    internal static ColoredStringBuilder AppendElem(this ColoredStringBuilder sb, Expression ce, VarDictIndex numIndex)
    {
        ce.ToString(sb, numIndex);
        return sb;
    }

    internal static ColoredStringBuilder ExtractElem(this ColoredStringBuilder sb, Expression ce, VarDictIndex numIndex)
    {
        numIndex.GetText(sb, ce);
        return sb;
    }

    internal static ColoredStringBuilder ExtractElem(this ColoredStringBuilder sb, Fonction ce, FuncDictIndex numIndex)
    {
        numIndex.GetText(sb, ce);
        return sb;
    }

    internal static ColoredStringBuilder RegisterElem(this ColoredStringBuilder sb, Expression ce, RealDictIndex numIndex)
        => numIndex.RegisterElem(sb, ce);

    internal static ColoredStringBuilder RegisterElem(this ColoredStringBuilder sb, Fonction ce, RealFuncDictIndex numIndex)
        => numIndex.RegisterElem(sb, ce);

    internal static ColoredStringBuilder AppendTabs(this ColoredStringBuilder sb, int count)
        => sb.Append(new string(' ', 3 * count));
}
global using System;
global using System.Collections.Generic;
using Model;
using System.Linq;

namespace Evaluator;

internal record struct Context(int[] Param)
{
    internal Context(int[] param, ExternalSource simpleExternal, ExternalSource uniqueExternal, ExternalSource orderedExternal)
        : this(param)
    {
        SimpleExternal = simpleExternal;
        UniqueExternal = uniqueExternal;
        OrderedExternal = orderedExternal;
    }

    internal readonly ExternalSource SimpleExternal;
    internal readonly ExternalSource UniqueExternal;
    internal readonly ExternalSource OrderedExternal;
}

/// <summary>Classe permettant de simuler l'execution du code</summary>
public static class Evaluator
{
    /// <summary>Execute l'application donnée en paramètre</summary>
    /// <param name="app">L'application a executer</param>
    public static int Run(Application app)
        => Run(app, new DictionarySource(new()), new DictionarySource(new()), new ConsoleSource());

    /// <summary>Execute l'application donnée en paramètre</summary>
    /// <param name="app">L'application a executer</param>
    /// <param name="simpleExternal">La valeur des simple external</param>
    /// <param name="uniqueExternal">La valeur des unique external</param>
    /// <param name="orderedExternal">La valeur des ordered external</param>
    public static int Run(Application app, ExternalSource simpleExternal, ExternalSource uniqueExternal, ExternalSource orderedExternal)
        => Evaluate(app.Main, new Context(Array.Empty<int>(), simpleExternal, uniqueExternal, orderedExternal));

    private static int Evaluate(Fonction f, Context ctx)
    {
        Dictionary<Expression, int> valeurs = new();
        int current = 0;
        foreach (Expression item in f.Actions)
        {
            current = Evaluate(item, valeurs, ctx);
            valeurs[item] = current;
        }

        return current;
    }

    private static int Evaluate(Expression expr, Dictionary<Expression, int> valeurs, Context ctx)
    {
        return expr switch
        {
            AdditionExpression ae => valeurs[ae.First] + valeurs[ae.Second],
            ConditionalExpression ce => valeurs[ce.Cond] == 0
                ? Evaluate(ce.ZeroBranch, valeurs, ctx)
                : Evaluate(ce.NonZeroBranc, valeurs, ctx),
            ConstExpression ce => ce.Valeur,
            SimpleExternal se => ctx.SimpleExternal.GetValue(se.Valeur),
            UniqueExternal ue => ctx.UniqueExternal.GetValue(ue.Valeur),
            OrderedExternal oe => ctx.OrderedExternal.GetValue(oe.Valeur),
            FuncCall fc
            => Evaluate(fc.Func, ctx with { Param = fc.Param.Select(item => valeurs[item]).ToArray() }),
            ParamExpression pe => ctx.Param[pe.Valeur],
            _ => throw new Exception(),
        };
    }
}
using Model;
using System;
using static Evaluator.Evaluator;

namespace TestOptimisation;

/// <summary>Application entry point</summary>
public static class Program
{
    public static void Main()
    {
        Console.WriteLine("Calcul de la somme des entiers inférieurs");
        Application simple = SommeSimple();
        simple.Print();
        Console.WriteLine($"Resultat : {Run(simple)}");

        Console.WriteLine("Calcul de la somme des entiers inférieurs (optimisé)");
        Application opti = SommeOpti();
        opti.Print();
        Console.WriteLine($"Resultat : {Run(opti)}");
    }

    private static Application SommeSimple()
    {
        ParamExpression param = new(0);
        ConstExpression valeurSous = new(-1);
        AdditionExpression sous = new(param, valeurSous);
        FuncCall<Args1> recursiveCall = FuncCall.Create(new Fonction<Args1>(new InvalidExpression()), sous);
        ConditionalExpression cond = new(sous, new ConstExpression(0), recursiveCall);
        AdditionExpression add = new(param, cond);

        Fonction<Args1> sum = new(valeurSous, param, sous, cond, add);
        recursiveCall.Func = sum;

        OrderedExternal mainRead = new(0);
        FuncCall<Args1> mainCall = FuncCall.Create(sum, mainRead);

        return new(new Fonction<Args0>(mainRead, mainCall), sum);
    }

    private static Application SommeOpti()
    {
        ParamExpression param = new(0);
        ConstExpression valeurSous = new(-1);
        AdditionExpression sous = new(param, valeurSous);
        FuncCall<Args1> recursiveCall = FuncCall.Create(new Fonction<Args1>(new InvalidExpression()), sous);
        AdditionExpression add = new(param, recursiveCall);

        Fonction<Args1> recursion = new(valeurSous, param, sous, recursiveCall, add);

        ParamExpression param2 = new(0);
        ConditionalExpression cond = new(param2, new ConstExpression(0), FuncCall.Create(recursion, param2));

        Fonction<Args1> sum = new(param2, cond);

        recursiveCall.Func = sum;

        OrderedExternal mainRead = new(0);
        FuncCall<Args1> mainCall = FuncCall.Create(sum, mainRead);

        return new(new Fonction<Args0>(mainRead, mainCall), sum, recursion);
    }
}
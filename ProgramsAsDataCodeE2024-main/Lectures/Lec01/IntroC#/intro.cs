using System;

namespace CsharpShooter
{
    abstract class Expr
    {
        public abstract override string ToString();
    }

    class CstI : Expr
    {
        public int Value { get; }

        public CstI(int value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value.ToString();
        }
    }

    class Var : Expr
    {
        public string Name { get; }

        public Var(string name)
        {
            Name = name;
        }

        public override string ToString()
        {
            return Name;
        }
    }

    abstract class Binop : Expr
    {
        public Expr Left { get; }
        public Expr Right { get; }

        protected Binop(Expr left, Expr right)
        {
            Left = left;
            Right = right;
        }
    }

    class Add : Binop
    {
        public Add(Expr left, Expr right) : base(left, right) { }

        public override string ToString()
        {
            return $"({Left} + {Right})";
        }
    }

    class Mul : Binop
    {
        public Mul(Expr left, Expr right) : base(left, right) { }

        public override string ToString()
        {
            return $"({Left} * {Right})";
        }
    }

    class Sub : Binop
    {
        public Sub(Expr left, Expr right) : base(left, right) { }

        public override string ToString()
        {
            return $"({Left} - {Right})";
        }
    }

    class Program
    {
        static void Main()
        {
            Expr e = new Add(new CstI(17), new Var("z"));
            Console.WriteLine(e.ToString());

        }
    }
}
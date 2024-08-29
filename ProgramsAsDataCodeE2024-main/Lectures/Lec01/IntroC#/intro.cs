using System;

namespace CsharpShooter
{
    
    //Our prior data type in F#, expr, is now an abstract class.
    
    abstract class Expr
    {
        //abstract methods promises to be used in its child classes.
        public abstract override string ToString();

        public abstract int eval (Dictionary<string, int> env);
    }

    //It's either a constant ...
    class CstI : Expr
    {
        public int Value { get; }

        public CstI(int value)
        {
            Value = value;
        }

        public override int eval (Dictionary<string, int> env)
        {
            return Value;
        }

        public override string ToString()
        {
            return Value.ToString();
        }
    }
    
    //... or a variable ...
    class Var : Expr
    {
        public string Name { get;}

        public Var(string name)
        {
            Name = name;
        }

        public override int eval(Dictionary<string, int> env)
        {
            return env[Name];
        }

        public override string ToString()
        {
            return Name;
        }
    }

    // the binary operations is with, fucking, either Expr or Var, y'know? 
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

        public override int eval(Dictionary<string, int> env)
        {
            var leftVal = Left.eval(env);
            var rightVal = Right.eval(env);
            return leftVal + rightVal;

        }

        public override string ToString()
        {
            return $"({Left} + {Right})";
        }
    }

    class Mul : Binop
    {
        public Mul(Expr left, Expr right) : base(left, right) { }

        public override int eval(Dictionary<string, int> env)
        {
            var leftVal = Left.eval(env);
            var rightVal = Right.eval(env);
            return leftVal * rightVal;
        }

        public override string ToString()
        {
            return $"({Left} * {Right})";
        }
    }

    class Sub : Binop
    {
        public Sub(Expr left, Expr right) : base(left, right) { }

        public override int eval(Dictionary<string, int> env)
        {
            var leftVal = Left.eval(env);
            var rightVal = Right.eval(env);
            return leftVal - rightVal;
        }

        public override string ToString()
        {
            return $"({Left} - {Right})";
        }
    }

    class Program
    {
        static void Main()
        {
            Expr e1 = new Add(new CstI(17),new Sub(new Var("x"), new Var("y")));
            Expr e2 = new Mul(new CstI(17), new CstI(3));
            Expr e3 = new Sub(new Var("x"), new Var("y"));
            Console.WriteLine(e1.ToString());
            Console.WriteLine(e2.ToString());
            Console.WriteLine(e3.ToString());
            
            // stack machine
            var env = new Dictionary<string, int>
            {
                { "x", 6},
                { "y", 7}
            };
            
            //expr to be evaluated
            
            //NOTE: We look for variables in our env, if the variable is not in the env,
            //we die.
            Expr myExpr = new Add(new Var("x"), new CstI(10));
            
            //evaluted result
            var res = myExpr.eval(env);
            Console.WriteLine(res);


        }
    }
}
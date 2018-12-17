using System;

namespace typeCase
{
    public static class Evaluator
    {
        public static int Evaluate(Expr that)
        {
            var c = that as Const;
            if (c != null) return c.info;
            var a = that as Add;
            if (a != null) return Evaluate(a.left) + Evaluate(a.right);
            throw new ArgumentException();
        }
    }
}

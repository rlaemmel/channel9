namespace extensionMethods
{
    public static class Evaluator
    {
        public static int Evaluate(this Expr that)
        {
            return 0; // requires type-case
        }
        public static int Evaluate(this Const that) 
        { 
            return that.info; 
        }
        public static int Evaluate(this Add that)
        {
            return that.left.Evaluate() + that.right.Evaluate();
        }
    }
}

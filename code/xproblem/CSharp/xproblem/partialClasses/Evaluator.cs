namespace partialClasses
{
    public abstract partial class Expr
    {
        public abstract int Evaluate();
    }
    public partial class Const : Expr
    {
        public override int Evaluate() 
        { 
            return info; 
        }
    }
    public partial class Add : Expr
    {
        public override int Evaluate() 
        {
            return left.Evaluate() + right.Evaluate(); 
        }
    }
}

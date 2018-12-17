namespace partialClasses
{
    public abstract partial class Expr
    { 
    }
    public partial class Const : Expr
    {
        public int info;
    }
    public partial class Add : Expr
    {
        public Expr left, right;
    }
}

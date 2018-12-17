namespace partialClasses
{
    public abstract partial class Expr
    {
        public abstract string PrettyPrint();
    }
    public partial class Const : Expr
    {
        public override string PrettyPrint() 
        { 
            return info.ToString(); 
        }
    }
    public partial class Add : Expr
    {
        public override string PrettyPrint() 
        {
            return left.PrettyPrint() + " + " + right.PrettyPrint(); 
        }
    }
}

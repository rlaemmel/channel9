namespace extensionMethods
{
    public abstract class Expr
    { 
    }
    public class Const : Expr
    {
        public int info;
    }
    public class Add : Expr
    {
        public Expr left, right;
    }
}

namespace firstAttempt
{
    public abstract class Expr
    {
        public abstract string PrettyPrint();
    }
    public class Const : Expr
    {
        public int info;
        public override string PrettyPrint()
        {
            return info.ToString();
        }
    }
    public class Add : Expr
    {
        public Expr left, right;
        public override string PrettyPrint()
        {
            return left.PrettyPrint() + " + " + right.PrettyPrint();
        }
    }
}

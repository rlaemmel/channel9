namespace visitorPattern
{
    public abstract class Expr
    {
        public abstract R Accept<R>(Visitor<R> v);
    }
    public class Const : Expr
    {
        public int info;
        public override R Accept<R>(Visitor<R> v) 
        {
            return v.Visit(this); 
        }
    }
    public class Add : Expr
    {
        public Expr left, right;
        public override R Accept<R>(Visitor<R> v)
        { 
            return v.Visit(this); 
        }
    }
}

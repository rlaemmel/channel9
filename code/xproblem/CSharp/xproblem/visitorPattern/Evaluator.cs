namespace visitorPattern
{
    public class Evaluator : Visitor<int>
    {
        public int Visit(Const that)
        {
            return that.info;
        }
        public int Visit(Add that)
        {
            return that.left.Accept(this) + that.right.Accept(this);
        }
    }
}

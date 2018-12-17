namespace visitorPattern
{
    public class PrettyPrinter : Visitor<string>
    {
        public string Visit(Const that) 
        { 
            return that.info.ToString(); 
        }
        public string Visit(Add that)
        {
            return that.left.Accept(this) + " + " + that.right.Accept(this); 
        }
    }
}

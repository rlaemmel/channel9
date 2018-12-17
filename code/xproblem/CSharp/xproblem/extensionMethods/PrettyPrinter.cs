namespace extensionMethods
{
    public static class PrettyPrinter
    {
        public static string PrettyPrint(this Expr that)
        {
            return null; // requires type-case
        }
        public static string PrettyPrint(this Const that) 
        {
            return that.info.ToString();
        }
        public static string PrettyPrint(this Add that)
        {
            return that.left.PrettyPrint() + " + " + that.right.PrettyPrint();
        }
    }
}

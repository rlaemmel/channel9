namespace firstAttempt
{
    public class Neg : Expr
    {
        public Expr operand;
        public override string PrettyPrint()
        {
            return "- (" + operand.PrettyPrint() +")";
        }
    }
}

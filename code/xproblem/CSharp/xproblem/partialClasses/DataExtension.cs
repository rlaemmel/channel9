namespace partialClasses
{
    public partial class Neg : Expr
    {
        public Expr operand;
        public override string PrettyPrint()
        {
            return "- (" + operand.PrettyPrint() +")";
        }
        public override int Evaluate()
        {
            return - operand.Evaluate();
        }
    }
}

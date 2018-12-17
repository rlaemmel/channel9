using System;

namespace typeCase
{
    public class Neg : Expr
    {
        public Expr operand;
    }
    public class PrettyPrinterWithNeg : PrettyPrinter
    {
        public override string PrettyPrint(Expr that)
        {
            try
            {
                return base.PrettyPrint(that);
            }
            catch (ArgumentException)
            {
                var n = that as Neg;
                if (n != null) return "- (" + PrettyPrint(n.operand) + ")";
                throw new ArgumentException();
            }
        }
    }
}

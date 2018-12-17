using System;

namespace typeCase
{
    public class PrettyPrinter
    {
        public virtual string PrettyPrint(Expr that)
        {
            var c = that as Const;
            if (c != null) return c.info.ToString();
            var a = that as Add;
            if (a != null) return PrettyPrint(a.left) + "+" + PrettyPrint(a.right);
            throw new ArgumentException();
        }
    }
}

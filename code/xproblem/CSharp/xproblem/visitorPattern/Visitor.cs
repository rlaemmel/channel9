using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace visitorPattern
{
    public interface Visitor<R>
    {
        R Visit(Const that);
        R Visit(Add that);
    }
}

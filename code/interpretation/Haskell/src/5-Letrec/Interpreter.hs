#include "../4-Lambda/Interpreter.hs"

interpret (Letrec n x1 x2) e
 = interpret x2 (fix (modify e n . interpret x1))

package typeCase;

/**
 * The expression form of negation
 */
public class Neg extends Expr {
	private Expr expr;
	public Expr getExpr() { return expr; }
	public void setExpr(Expr expr) { this.expr = expr; }	
}

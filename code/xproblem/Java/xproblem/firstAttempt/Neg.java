package firstAttempt;

/**
 * The expression form of negation
 */
public class Neg extends Expr {
	
	private Expr expr;
	public Expr getExpr() { return expr; }
	public void setExpr(Expr expr) { this.expr = expr; }	
	
	public String prettyPrint() {
		return "-(" + getExpr().prettyPrint() + ")";
	}
	
	public int evaluate() {
		return - getExpr().evaluate();
	}
}

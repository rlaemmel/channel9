package firstAttempt;

/**
 * The expression form of addition
 */
public class Add extends Expr {
	
	private Expr left, right;
	public Expr getLeft() { return left; }
	public void setLeft(Expr left) { this.left = left; }	
	public Expr getRight() { return right; }
	public void setRight(Expr right) { this.right = right; }	
	
	public String prettyPrint() {
		return getLeft().prettyPrint() + " + " + getRight().prettyPrint();
	}
	
	public int evaluate() {
		return getLeft().evaluate() + getRight().evaluate();
	}
}

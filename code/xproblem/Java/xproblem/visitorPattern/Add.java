package visitorPattern;

/**
 * The expression form of addition
 */
public class Add extends Expr {
	
	private Expr left, right;
	public Expr getLeft() { return left; }
	public void setLeft(Expr left) { this.left = left; }	
	public Expr getRight() { return right; }
	public void setRight(Expr right) { this.right = right; }	
	
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}
}

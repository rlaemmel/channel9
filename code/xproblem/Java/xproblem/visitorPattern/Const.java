package visitorPattern;

/**
 * The expression form of constants
 */
public class Const extends Expr {
	
	private int info;
	public int getInfo() { return info; }
	public void setInfo(int info) { this.info = info; }
	
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}
}

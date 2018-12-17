package firstAttempt;

/**
 * The expression form of integer constants
 */
public class Const extends Expr {

	private int info;
	public int getInfo() { return info; }
	public void setInfo(int info) { this.info = info; }
	
	public String prettyPrint() {
		return Integer.toString(getInfo());
	}
	
	public int evaluate() {
		return getInfo();
	}
}

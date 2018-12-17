package visitorPattern;

/**
 * The base class of all expression forms
 */
public abstract class Expr {
	
	/* 
	 * Accept a visitor (i.e., apply an operation)
	 */ 
	public abstract <R> R accept(Visitor<R> v);
}

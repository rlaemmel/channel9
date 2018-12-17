package firstAttempt;

/**
 * The base class of all expression forms
 */
public abstract class Expr {
	
	/*
	 * Operation for pretty printing
	 */
	public abstract String prettyPrint();
	
	/*
	 * Operation for expression evaluation
	 */
	public abstract int evaluate();
}
package visitorPattern;

/**
 * Operation for expression evaluation
 */
public class Evaluator extends Visitor<Integer> {
	public Integer visit(Const x) {
		return x.getInfo();
	}
	public Integer visit(Add x) {
		return x.getLeft().accept(this) + x.getRight().accept(this);
	}
	public Integer visit(Neg x) {
		return - x.getExpr().accept(this);
	}
}

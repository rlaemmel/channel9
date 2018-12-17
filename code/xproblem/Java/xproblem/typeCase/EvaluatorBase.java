package typeCase;

public class EvaluatorBase {

	public int evaluate(Expr e) {
		if (e instanceof Const) {
			Const l = (Const)e;
			return l.getInfo();
		}
		if (e instanceof Add) {
			Add a = (Add)e;
			return evaluate(a.getLeft()) + evaluate(a.getRight());
		}		
		throw new FallThrouhException();
	}
}

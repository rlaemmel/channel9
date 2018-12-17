package typeCase;

public class EvaluatorExtension extends EvaluatorBase {

	public int evaluate(Expr e) {
		try {
			return super.evaluate(e);
		}
		catch (FallThrouhException x) {
			if (e instanceof Neg) {
				Neg n = (Neg)e;
				return -evaluate(n.getExpr());
			}		
			throw new FallThrouhException();			
		}
	}
}

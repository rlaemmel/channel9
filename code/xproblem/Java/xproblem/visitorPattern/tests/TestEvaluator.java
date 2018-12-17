package visitorPattern.tests;

import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;
import visitorPattern.*;

public class TestEvaluator {

	private static Visitor<Integer> v;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		v = new Evaluator();
	}
	
	@Test
	public void testConst() {
		Const x = new Const();
		x.setInfo(42);
		assertEquals("evaluate a literal", 42, x.accept(v));
	}
	
	@Test
	public void testAdd() {
		Add x = new Add();
		Const y = new Const();
		y.setInfo(1);
		x.setLeft(y);
		y = new Const();
		y.setInfo(2);
		x.setRight(y);
		assertEquals("evaluate addition", 3, x.accept(v));
	}	
}

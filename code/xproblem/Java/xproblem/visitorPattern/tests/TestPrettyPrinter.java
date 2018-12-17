package visitorPattern.tests;

import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;
import visitorPattern.*;

public class TestPrettyPrinter {

	private static Visitor<String> v;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		v = new PrettyPrinter();
	}
	
	@Test
	public void testConst() {
		Const x = new Const();
		x.setInfo(42);
		assertEquals("pretty print a literal", "42", x.accept(v));
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
		assertEquals("pretty print addition", "1 + 2", x.accept(v));
	}	
}

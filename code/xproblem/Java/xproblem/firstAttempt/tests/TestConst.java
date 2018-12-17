package firstAttempt.tests;

import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;
import firstAttempt.*;

public class TestConst {

	private static Const x;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		x = new Const();
		x.setInfo(42);
	}
	
	@Test
	public void testPrettyPrint() {
		assertEquals("pretty print a constant", "42", x.prettyPrint());
	}

	@Test
	public void testEvaluate() {
		assertEquals("evaluate a constant", 42, x.evaluate());
	}
	
}

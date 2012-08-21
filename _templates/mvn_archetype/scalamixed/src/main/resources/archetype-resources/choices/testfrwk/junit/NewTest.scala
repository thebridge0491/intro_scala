#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.junit.Test
import org.junit.Assert._

class NewTest {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 1.0e-7f
    
    @org.junit.Before
    def setUp(): Unit = {
    	System.err.println("setup Test ...")
    }
    @org.junit.After
    def tearDown(): Unit = {
    	System.err.println("... teardown Test")
    }
	
    def in_epsilon(a: Double, b: Double, tolerance: Double = 0.001): Boolean = {
        val delta = Math.abs(tolerance)
        //(a - delta) <= b && (a + delta) >= b
		!((a + delta) < b) && !((b + delta) < a)
    }
	
    @Test
    def test_classExists() = {
        try {
            Class.forName("%s.Library".format(
                this.getClass.getPackage.getName))
            assertTrue(true)
        } catch {
            case exc: ClassNotFoundException => {
                //fail("Class(es) not existent: " + 
                //    classOf[Library].getName)
                fail("%s %s".format("Class(es) not existent:", 
                    Array(classOf[Library].getName).mkString))
            }
        }
    }
    
	@Test
    def test_method() = { assertEquals(4, 2 * 2)
    }
    @Test
    def test_dblMethod() = {
        //assertEquals(100.001f, 100.001f, epsilon)
        assertTrue(in_epsilon(100.001f, 100.001f, epsilon))
    }
    @Test
    def test_strMethod() = { assertEquals("Hello", "Hello")
    }
    @Test(timeout = 1000000)
    def test_timeoutMethod() = { for (i <- List.range(0, 1.0e6.toInt, 1)) {}
    }
    @org.junit.Ignore @Test
    def test_ignoredMethod() = { assertEquals(5, 2 * 2)
    }
    @Test //(expected = AssertionError.class)
    def test_failMethod() = { fail()
    }
    @Test(expected = classOf[IllegalArgumentException])
    def test_exceptionMethod() { throw new IllegalArgumentException
    }
}

object NewTest {
    @org.junit.BeforeClass
    def setUpClass(): Unit = {
    	System.err.println("${symbol_pound}${symbol_pound}${symbol_pound}setup TestCase${symbol_pound}${symbol_pound}${symbol_pound}")
    }
    @org.junit.AfterClass
    def tearDownClass(): Unit = {
    	System.err.println("${symbol_pound}${symbol_pound}${symbol_pound}teardown TestCase${symbol_pound}${symbol_pound}${symbol_pound}")
    }
}

}

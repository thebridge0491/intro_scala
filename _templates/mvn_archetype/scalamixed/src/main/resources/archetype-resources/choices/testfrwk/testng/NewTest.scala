#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.testng.annotations.Test
import org.testng.Assert._

class NewTest {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 1.0e-7f
    
    @org.testng.annotations.BeforeMethod
    def setUp(): Unit = {
    	System.err.println("setup Test ...")
    }
    @org.testng.annotations.AfterMethod
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
	
	@Test(groups = Array[String]("grp1"))
    def test_method() = { assertEquals(4, 2 * 2)
    }
    @Test(groups = Array[String]("grp1"))
    def test_dblMethod() = {
        //assertEquals(100.001f, 100.001f, epsilon)
        assertTrue(in_epsilon(100.001f, 100.001f, epsilon))
    }
    @Test
    def test_strMethod() = { assertEquals("Hello", "Hello")
    }
    @Test(timeOut = 1000000)
    def test_timeoutMethod() = { for (i <- List.range(0, 1.0e6.toInt, 1)) {}
    }
    @Test(enabled = false)
    def test_ignoredMethod() = { assertEquals(5, 2 * 2)
    }
    @Test //(expectedExceptions = Array[_](AssertionError.class))
    def test_failMethod() = { fail()
    }
    @Test(expectedExceptions = Array[IllegalArgumentException](classOf[IllegalArgumentException]))
    def test_exceptionMethod() { throw new IllegalArgumentException
    }
}

object NewTest {
    @org.testng.annotations.BeforeClass
    def setUpClass(): Unit = {
    	System.err.println("${symbol_pound}${symbol_pound}${symbol_pound}setup TestCase${symbol_pound}${symbol_pound}${symbol_pound}")
    }
    @org.testng.annotations.AfterClass
    def tearDownClass(): Unit = {
    	System.err.println("${symbol_pound}${symbol_pound}${symbol_pound}teardown TestCase${symbol_pound}${symbol_pound}${symbol_pound}")
    }
}

}

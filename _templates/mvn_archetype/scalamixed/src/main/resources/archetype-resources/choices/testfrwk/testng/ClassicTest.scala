#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.testng.annotations.Test
import org.testng.Assert._

class ClassicTest {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
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
	
	def cartesian_prod[T: Manifest](arr1: Array[T], arr2: Array[T]):
			Array[Array[T]] = {
		//val prod_arrs = for {a <- arr1 ; b <- arr2} yield Array[T](a, b)
        val prod_arrs = arr1.flatMap(a => arr2.map(b => Array[T](a, b)))
        prod_arrs
	}
	
    @Test(groups = Array[String]("grp1"))
    def test_fact() = {
    	val funcs = Array[(Long) => Long](Classic.fact_i, Classic.fact_lp)
    	
        for (f <- funcs)
        	assertEquals(120L, f(5))
    }
    
    @Test(groups = Array[String]("grp1"))
    def test_expt() = {
    	val funcs = Array[(Float, Float) => Float](Classic.expt_i,
            Classic.expt_lp)
        val param1 = Array[Float](2.0f, 11.0f, 20.0f)
        val param2 = Array[Float](3.0f, 6.0f, 10.0f)
        val prod_params = cartesian_prod[Float](param1, param2)
        
        for (f <- funcs)
        	for (row <- prod_params) {
        		val exp = math.pow(row(0), row(1))
        		assertEquals(exp, f(row(0), row(1)), exp * epsilon)
        	}
    }
}

object ClassicTest {
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

package org.sandbox.intro_scala.foreignc {

//import org.scalatest._

import org.sandbox.intro_scala.util.{Library => Util}

class ClassicTest extends UnitSpec {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    behavior of "Classic functions test(s)"
    
    before {
		System.err.println("###setup TestCase###")
	}
	after {
		System.err.println("###teardown TestCase###")
	}
    override def beforeEach() : Unit = {
		System.err.println("setup Test ...")
	}
	override def afterEach() : Unit = {
		System.err.println("... teardown Test")
	}
	
    it should "compute factorial" taggedAs(Tag1) in {
		val funcs = Array[(Long) => Long](Classic.fact_i, Classic.fact_lp)
        for (f <- funcs)
        	assertResult(120L) { f(5) }
	}
    
    it should "compute exponent" taggedAs(Tag1) in {
		val funcs = Array[(Float, Float) => Float](Classic.expt_i,
            Classic.expt_lp)
        val param1 = Array[Float](2.0f, 11.0f, 20.0f)
        val param2 = Array[Float](3.0f, 6.0f, 10.0f)
        val prod_params = Util.cartesian_prod[Float](param1, param2)
        
        for (f <- funcs)
        	for (row <- prod_params) {
        		val exp = math.pow(row(0), row(1))
				assertResult(true) { 
					Util.in_epsilon(exp, f(row(0), row(1)), exp * epsilon) }
        	}
	}
}

object ClassicTest {
    
}

}

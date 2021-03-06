#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._
import org.testng.annotations.Test
import org.testng.Assert._

class ClassicProp {
	import scala.language.implicitConversions
	import org.scalacheck.{Test => SchkTest}

	implicit def doCheck(p: org.scalacheck.Prop): Boolean = {
		SchkTest.check(SchkTest.Parameters.default, p).passed
        //SchkTest.check(SchkTest.Parameters.defaultVerbose, p).passed
	}
    
    // using def doCheck, scalacheck property check
    /*val propMap = ClassicProp.properties.toMap
    
    @Test(groups = Array[String]("grp1"))
    def test_propFact() = assertTrue(doCheck(
        propMap(ClassicProp.name + ".factorial n")))
    
    @Test(groups = Array[String]("grp1"))
    def test_propExpt() = assertTrue(doCheck(
        propMap(ClassicProp.name + ".exponent b to n")))*/
    
    @Test(groups = Array[String]("grp2"))
    def test_propsClassic() =
        // (from testng) execute scalacheck-style propert(y|ies) check(s)
        //ClassicProp.main(Array())
        ClassicProp.properties.foreach { (name_prop:(String, Prop)) => 
            name_prop match { case (name, prop) => 
                //prop.check
                
                // using def doCheck, scalacheck property check
                try {
                    assertTrue(doCheck(prop))
                } catch {
                    case exc: AssertionError => {
                        Console.err.printf("AssertionError: %s\n".format(name))
                        fail("%s -- %s".format(name, prop))
                    }
                }
            }
        }
}

// scalacheck properties object w/ (implicit) main method
object ClassicProp extends Properties("(props) Classic functions") {
    //import scala.language.implicitConversions
    
    implicit val chooseInteger: Gen.Choose[Integer] = new Gen.Choose[Integer] {
		def choose(low: Integer, high: Integer) =
			Gen.Choose.chooseInt.choose(low.intValue, high.intValue).map(
				e => Integer.valueOf(e))
	}
    
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
	
    def in_epsilon(a: Double, b: Double, tolerance: Double = 0.001): Boolean = {
        val delta = Math.abs(tolerance)
        //(a - delta) <= b && (a + delta) >= b
		!((a + delta) < b) && !((b + delta) < a)
    }
    
    def genTup2Float(gen0: Gen[Int], gen1: Gen[Int]) = 
        for { x <- gen0.map(_.toFloat) ; y <- gen1.map(_.toFloat)
            } yield (x, y)
    
    
    // scalacheck-style property define
    property("factorial n") = forAll(Gen.choose(0, 18)) { n =>
		val ans: Long = List.range(1, n + 1).foldLeft(1L)(_ * _)
		val funcs = Array[(Long) => Long](Classic.fact_i, Classic.fact_lp)
        (funcs.foldLeft(true) { (acc, f) => acc && (ans == f(n)) }).label(
			"===propFact(%d) : %d===".format(n, ans))
	}
    
    property("exponent b to n") = forAll(genTup2Float(Gen.choose(1, 20),
            Gen.choose(1, 10))) { (b_n:(Float, Float)) => 
		b_n match { case (b, n) =>
			val ans = Math.pow(b, n).toFloat
			val funcs = Array[(Float, Float) => Float](Classic.expt_i, 
                Classic.expt_lp)
			(funcs.foldLeft(true) { (acc, f) => acc && 
				in_epsilon(ans, f(b, n), ans * epsilon) }).
				label("===propExpt(%f, %f) : %f===".format(b, n, ans))
		}
	}
}

}

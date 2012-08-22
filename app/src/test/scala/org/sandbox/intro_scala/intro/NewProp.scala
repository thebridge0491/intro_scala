package org.sandbox.intro_scala.intro {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._

import scala.collection.mutable.{ListBuffer => M_List}

import org.sandbox.intro_scala.util.{Library => Util}

class NewProp extends UnitPropSpec {
	import scala.language.implicitConversions
	import org.scalacheck.{Test => SchkTest}

	implicit def doCheck(p: org.scalacheck.Prop): Boolean = {
		SchkTest.check(SchkTest.Parameters.default, p).passed
        //SchkTest.check(SchkTest.Parameters.defaultVerbose, p).passed
	}
    
    // (from scalatest) execute scalacheck-style propert(y|ies) check(s)
    //NewProp.main(Array())
    NewProp.properties.foreach { (name_prop:(String, Prop)) => 
        name_prop match { case (name, prop) => 
            //prop.check
            
            // scalatest-style property check
            //property("stest " + name, Tag2) { check { prop }}
            
            // using def doCheck, scalacheck property check
            property(name, Tag2) { assertResult(true) { doCheck(prop) }}
        }
    }
}

// scalacheck properties object w/ (implicit) main method
object NewProp extends Properties("(props) New examples") {
    //import scala.language.implicitConversions
    
    implicit val chooseInteger: Gen.Choose[Integer] = new Gen.Choose[Integer] {
		def choose(low: Integer, high: Integer) =
			Gen.Choose.chooseInt.choose(low.intValue, high.intValue).map(
				e => Integer.valueOf(e))
	}
    
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    def genTup2Int(gen0: Gen[Int], gen1: Gen[Int]) = 
        for { x <- gen0 ; y <- gen1 } yield (x, y)
    
    def genTup3Int(gen0: Gen[Int], gen1: Gen[Int], gen2: Gen[Int]) = 
        for { x <- gen0 ; y <- gen1 ; z <- gen2 } yield (x, y, z)
    
    def genListInts(gen0: Gen[Int]) = for {
        numElems <- Gen.choose[Int](1, 100)
        //elems <- Gen.listOfN[Int](numElems, gen0)
		elems <- Gen.containerOfN[M_List, Int](numElems, gen0)
	} yield elems
    
    
    // scalacheck-style property define
    property("commutative addition") = forAll(genTup2Int(
            Gen.choose(-50, 50), Gen.choose(-50, 50))) { (x_y:(Int, Int)) =>
        x_y match { case (x, y) =>
            val (ans, res) = (x + y, y + x)
            (ans == res).label("===propCommutativeAdd(%d, %d) : %d===".format(
                x, y, ans))
        }
    }
    
    property("associative addition") = forAll(genTup3Int(
            Gen.choose(-25, 25), Gen.choose(-25, 25), Gen.choose(0, 25))) { (x_y_z:(Int, Int, Int)) =>
        x_y_z match { case (x, y, z) =>
            val (ans, res) = ((x + y) + z, x + ( y + z))
            (ans == res).label("===propCommutativeAdd(%d, %d, %d) : %d===".format(
                x, y, z, ans))
        }
    }
    
    property("list reverse reverse is list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: M_List[Int]) => 
        val (ans, res) = (xs, xs.reverse.reverse)
        (ans == res).label("===propRevRev(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("list reverse is list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: M_List[Int]) => 
        val (ans, res) = (xs, xs.reverse)
        (ans == res).label("===propRevId(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("list reverse & sorted is list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: M_List[Int]) => 
        val (ans, res) = (xs.sorted, xs.reverse.sorted)
        (ans == res).label("===propSortRev(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("head of sorted list is minimum") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: M_List[Int]) => 
        val (ans, res) = (xs.min, xs.sorted.apply(0))
        (ans == res).label("===propSortMin(%d) : %d===".format(res, ans))
    }
}

}

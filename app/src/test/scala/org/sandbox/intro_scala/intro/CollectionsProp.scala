package org.sandbox.intro_scala.intro {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._

import org.sandbox.intro_scala.util.{Library => Util}

class CollectionsProp extends UnitPropSpec {
	import scala.language.implicitConversions
	import org.scalacheck.{Test => SchkTest}

	implicit def doCheck(p: org.scalacheck.Prop): Boolean = {
		SchkTest.check(SchkTest.Parameters.default, p).passed
        //SchkTest.check(SchkTest.Parameters.defaultVerbose, p).passed
	}
    
    // (from scalatest) execute scalacheck-style propert(y|ies) check(s)
    //CollectionsProp.main(Array())
    CollectionsProp.properties.foreach { (name_prop:(String, Prop)) => 
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
object CollectionsProp extends Properties("(props) Collections ops") {
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
		elems <- Gen.containerOfN[List, Int](numElems, gen0)
	} yield elems
    
    
    // scalacheck-style property define
    property("list equals list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val (ans, res) = (xs, xs.map(identity))
        (ans == res).label("===propEqual(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("list append elem not equals list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val (ans, res) = (xs :+ 100, xs.map(identity))
        (ans != res).label("===propNotEqual(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("list1 append list2 equals (list1 + list2)") = forAll(genListInts(
            Gen.choose(-50, 50)), genListInts(Gen.choose(-50, 50))) {
            (xs: List[Int], ys: List[Int]) => 
        val combo = xs ++ ys
        val (resX, resY) = combo.splitAt(xs.size)
        (xs == resX && ys == resY).label("===propAppend(%s, %s) : %s===".format(
            resX.mkString("[", ", ", "]"), resY.mkString("[", ", ", "]"), 
            combo.mkString("[", ", ", "]")))
    }
    
    property("list reverse reverse is list") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val (ans, res) = (xs, xs.reverse.reverse)
        (ans == res).label("===propRevRev(%s) : %s===".format(
            res.mkString("[", ", ", "]"), ans.mkString("[", ", ", "]")))
    }
    
    property("list filter") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val boolOp1 = ((e: Int) => 0 == (e % 2))
        val xsFilter = xs.filter(boolOp1)
        val ans = xsFilter.filterNot(boolOp1).isEmpty && xsFilter.forall(boolOp1)
        (ans).label("===propFilter(%s) : %s===".format(
            xsFilter.mkString("[", ", ", "]"), xs.mkString("[", ", ", "]")))
    }
    
    property("list map") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val proc1 = ((n: Int) => n + 2)
        val xsMap = xs.map(proc1)
        val ans = xs.foldLeft((true, xsMap))((a_mss:(Boolean, List[Int]),
            e: Int) => (a_mss._1 && a_mss._2(0) == proc1(e),
            a_mss._2.drop(1)))._1
        (ans).label("===propMap(%s) : %s===".format(
            xsMap.mkString("[", ", ", "]"), xs.mkString("[", ", ", "]")))
    }
    
    def isOrdered_i[T <% Ordered[T]](coll: Iterable[T],
            isRev: Boolean = false): Boolean = {
        def iter(acc: Boolean, rst: Iterable[T]): Boolean = rst match {
            case Nil | _ :: Nil => acc
            case x :: y :: ys => if (!isRev) iter(acc && x <= y, y :: ys) else iter(acc && x >= y, y :: ys)
        }
        iter(true, coll)
    }
    
    property("list sorted is ordered") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val xsOrdered = xs.sorted
        (isOrdered_i[Int](xsOrdered)).label("===propSorted(%s) : %s===".format(
            xsOrdered.mkString("[", ", ", "]"), xs.mkString("[", ", ", "]")))
    }
    
    property("list rev sorted is rev ordered") = forAll(genListInts(
            Gen.choose(-50, 50))) { (xs: List[Int]) => 
        val xsRevOrdered = xs.sorted.reverse
        (isOrdered_i[Int](xsRevOrdered, true)).label("===propRevSorted(%s) : %s===".format(
            xsRevOrdered.mkString("[", ", ", "]"), xs.mkString("[", ", ", "]")))
    }
}

}

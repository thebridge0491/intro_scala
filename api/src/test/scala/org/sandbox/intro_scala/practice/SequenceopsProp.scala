package org.sandbox.intro_scala.practice {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._
import java.util.Comparator
import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer => M_List}

import org.sandbox.intro_scala.util.{Library => Util}

class SequenceopsProp extends UnitPropSpec {
	import scala.language.implicitConversions
	import org.scalacheck.{Test => SchkTest}

	implicit def doCheck(p: org.scalacheck.Prop): Boolean = {
		SchkTest.check(SchkTest.Parameters.default, p).passed
        //SchkTest.check(SchkTest.Parameters.defaultVerbose, p).passed
	}
    
	// (from scalatest) execute scalacheck-style propert(y|ies) check(s)
    //SequenceopsProp.main(Array())
    SequenceopsProp.properties.foreach { (name_prop:(String, Prop)) => 
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
object SequenceopsProp extends Properties("(props) Sequence ops") {
    //import scala.language.implicitConversions
    
    implicit val chooseInteger: Gen.Choose[Integer] = new Gen.Choose[Integer] {
		def choose(low: Integer, high: Integer) =
			Gen.Choose.chooseInt.choose(low.intValue, high.intValue).map(
				e => Integer.valueOf(e))
	}
    
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    def genListInts(gen0: Gen[Integer]) = for {
        numElems <- Gen.choose[Int](1, 100)
        //elems <- Gen.listOfN[Integer](numElems, gen0)
		elems <- Gen.containerOfN[M_List, Integer](numElems, gen0)
	} yield elems
    
    
    // scalacheck-style property define
    property("find index") = forAll(genListInts(Gen.choose(0, 100)),
            Gen.choose[Integer](0, 100)) { 
		(xs: M_List[Integer], el: Integer) => 
			val arr = xs.toArray
            val (ansL, ansA) = (xs.indexOf(el), arr.indexOf(el))
			val funcsL = Array[(Integer, java.util.List[Integer], Comparator[Integer]) =>
                    Int](Sequenceops_java.indexOf_lp[Integer],
				Sequenceops.indexOf_lp[Integer])
			val funcsA = Array[(Integer, Array[Integer], Comparator[Integer]) => Int](
				Sequenceops_java.indexOf_lp[Integer],
                Sequenceops.indexOf_lp[Integer],
                Sequenceops.indexOf_i[Integer])
			
            (funcsL.foldLeft(true) { (acc, f) => acc && 
				(ansL == f(el, xs.asJava, Util.intCmp)) }).label(
				"===propIndexOfL(%d) : %d===".format(el, ansL)) &&
            (funcsA.foldLeft(true) { (acc, f) => acc && 
				(ansA == f(el, arr, Util.intCmp)) }).label(
				"===propIndexOfA(%d) : %d===".format(el, ansA))
	}
    
    property("reverse sequence") = forAll(genListInts(Gen.choose(0, 100))) { 
		(xs: M_List[Integer]) => 
			val arr = xs.toArray
            val (ansL, ansA) = (xs.map(identity).reverse, 
                arr.map(identity).reverse)
			val funcsL = Array[(java.util.List[Integer]) => Unit](
				Sequenceops_java.reverse_lp[Integer], Sequenceops.reverse_i[Integer])
            val funcsA = Array[(Array[Integer]) => Unit](
				Sequenceops_java.reverse_lp[Integer],
                Sequenceops.reverse_lp[Integer])
			var mutCopyL: M_List[Integer] = null //xs.map(identity)
			val imCopyL = Sequenceops.reverse_i[Integer](M_List[Integer](xs: _*): 
                collection.Seq[Integer]) //.asInstanceOf[M_List[Integer]]
            var mutCopyA: Array[Integer] = null //arr.map(identity)
            val imCopyA1 = Sequenceops.reverse_i[Integer](arr)
			val imCopyA2 = Sequenceops.reverse_r[Integer](arr)
			
            (funcsL.foldLeft(true) { (acc, f) => mutCopyL = xs.map(identity) ;
				f(mutCopyL.asJava) ; acc && 
				(ansL == mutCopyL) }).label(
				"===mut. propReverseL(%s) : %s  %s===".format(
				xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]"), 
				mutCopyL.mkString("[", ", ", "]"))) &&
			(ansL == imCopyL).label(
				"===immut. propReverseL(%s) : %s  %s===".format(
				xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]"), 
				imCopyL.mkString("[", ", ", "]"))) &&
            (funcsA.foldLeft(true) { (acc, f) => mutCopyA = arr.map(identity) ;
				f(mutCopyA) ; acc && (ansA sameElements mutCopyA) }).label(
				"===mut. propReverseA(%s) : %s  %s===".format(
				arr.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"), 
				mutCopyA.mkString("[", ", ", "]"))) &&
			(ansA.sameElements(imCopyA1) && ansA.sameElements(imCopyA2)).label(
				"===immut. propReverseA(%s) : %s  %s===".format(
				arr.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"), 
				imCopyA1.mkString("[", ", ", "]")))
	}
}

}

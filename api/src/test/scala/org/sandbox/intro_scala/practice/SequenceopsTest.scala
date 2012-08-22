package org.sandbox.intro_scala.practice {

//import org.scalatest._
import java.util.Comparator
import scala.collection.mutable.{ListBuffer => M_List}
import scala.collection.JavaConverters._

import org.sandbox.intro_scala.util.{Library => Util}

class SequenceopsTest extends UnitSpec {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    val (ints, ints_rev) = (Array[Integer](0, 1, 2, 3, 4),
    	Array[Integer](4, 3, 2, 1, 0))
    val (lst_ints, lst_ints_rev) = (M_List[Integer](ints: _*),
    	M_List[Integer](ints_rev: _*))
    
    behavior of "Sequence ops test(s)"
    
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
	
    it should "find index" taggedAs(Tag1) in {
    	val funcsL = List[(Integer, java.util.List[Integer], Comparator[Integer]) => Int](
    		Sequenceops_java.indexOf_lp[Integer], Sequenceops.indexOf_lp[Integer])
    	val funcsA = List[(Integer, Array[Integer], Comparator[Integer]) => Int](
    		Sequenceops_java.indexOf_lp[Integer],
            Sequenceops.indexOf_lp[Integer], Sequenceops.indexOf_i[Integer])
        val el: Int = 3
        
        for (f <- funcsL) {
        	assertResult(3) { f(el, lst_ints.asJava, Util.intCmp) }
            assertResult(1) { f(el, lst_ints_rev.asJava, Util.intCmp) }
        }
        for (f <- funcsA) {
        	assertResult(3) { f(el, ints, Util.intCmp) }
            assertResult(1) { f(el, ints_rev, Util.intCmp) }
        }
	}
    
    it should "reverse sequence" taggedAs(Tag1) in {
    	val funcsL = List[(java.util.List[Integer]) => Unit](
    		Sequenceops_java.reverse_lp[Integer], Sequenceops.reverse_i[Integer])
    	val funcsA = List[(Array[Integer]) => Unit](
    		Sequenceops_java.reverse_lp[Integer],
            Sequenceops.reverse_lp[Integer])
        
        for (f <- funcsL) {
        	val tmp = lst_ints.map(identity)
        	f(tmp.asJava)
        	
        	for (j <- 0 until tmp.size)
        		assertResult(lst_ints_rev(j)) { tmp(j) }
        }
        for (f <- funcsA) {
        	val tmp = ints.map(identity)
        	f(tmp)
        	
        	for (j <- 0 until tmp.length)
        		assertResult(ints_rev(j)) { tmp(j) }
        }
        
        val tmpL1 = Sequenceops.reverse_i[Integer](lst_ints: collection.Seq[Integer])   //.asInstanceOf[M_List[Integer]]
        val tmpA1 = Sequenceops.reverse_i[Integer](ints)
        val tmpA2 = Sequenceops.reverse_r[Integer](ints)
        
        for (j <- List.range(0, tmpL1.size))
    		assertResult(lst_ints_rev(j)) { tmpL1(j) }
        for (i <- 0 to (ints.length - 1)) {
        	assertResult(ints_rev(i)) { tmpA1(i) }
        	assertResult(ints_rev(i)) { tmpA2(i) }
        }
	}
}

object SequenceopsTest {
    
}

}

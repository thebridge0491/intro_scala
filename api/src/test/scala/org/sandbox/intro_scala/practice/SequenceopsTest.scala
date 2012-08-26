package org.sandbox.intro_scala.practice {

//import org.scalatest._
import java.util.Comparator
import scala.collection.mutable.Buffer
import scala.collection.JavaConverters._

import org.sandbox.intro_scala.util.{Library => Util}
import org.sandbox.intro_scala.practice.{Sequenceops => Seqops, 
	SequenceopsArray => SeqopsArr}

class SequenceopsTest extends UnitSpec {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    val (ints, ints_rev) = (Array[Integer](0, 1, 2, 3, 4),
    	Array[Integer](4, 3, 2, 1, 0))
    val (lst_ints, lst_ints_rev) = (List[Integer](ints: _*),
    	List[Integer](ints_rev: _*))
    
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
	
    it should "tabulate sequence" taggedAs(Tag2) in {
        Array(3, 5, 7).foreach { n =>
            val ansA1 = Array.range(0, n).foldLeft(Array[Int]())(
				(a, e) => e +: a).reverse
			val ansA2 = Array.range(0, n).foldLeft(Array[Int]())(
				(a, e) => (e + 2) +: a).reverse
            Array[((Int => Int), Int) => Array[Int]](SeqopsArr.tabulate_i,
                    SeqopsArr.tabulate_r).foreach { f =>
				assertResult(true) { ansA1 sameElements f(identity, n) }
				assertResult(true) { ansA2 sameElements f((i => i + 2), n) } }
			val ansL1 = List.range(0, n).foldLeft(List[Int]())(
				(a, e) => e :: a).reverse
			val ansL2 = List.range(0, n).foldLeft(List[Int]())(
				(a, e) => (e + 2) :: a).reverse
            List[((Int => Int), Int) => List[Int]](Seqops.tabulate_i, 
                    Seqops.tabulate_r).foreach { f =>
                assertResult(ansL1) { f(identity, n) }
				assertResult(ansL2) { f((i => i + 2), n) } }
        }
    }
    
    it should "compute length" taggedAs(Tag2) in {
        Array(3, 5, 7).foreach { n =>
            val arr1 = Array.range(0, n).map(e => e)
            Array[Array[Int] => Int](SeqopsArr.length_i, SeqopsArr.length_r).
                foreach { f => assertResult(arr1.size) { f(arr1) } }
            val lst1 = List.range(0, n).map(e => e)
            List[List[Int] => Int](Seqops.length_i, Seqops.length_r).
                foreach { f => assertResult(lst1.size) { f(lst1) } }
        }
    }
    
    it should "access nth item" taggedAs(Tag2) in {
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Int, Array[Integer]) => Option[Integer]](SeqopsArr.nth_i, 
                    SeqopsArr.nth_r).foreach { f =>
                assertResult(Some(arr(3))) { f(3, arr) } }
			List[(Int, List[Integer]) => Option[Integer]](Seqops.nth_i,
                    Seqops.nth_r).foreach { f => 
                assertResult(Some(lst(3))) { f(3, lst) } }
        }}
    }
    
    it should "find index|item" taggedAs(Tag1) in {
        val el: Int = 3
        
        Array((3, ints), (1, ints_rev)).foreach { i_arr => 
                i_arr match { case (i, arr) =>
            assertResult(i) { Sequenceops_java.indexOf_lp[Integer](el, arr,
                Util.intCmp) } }
        }
        List((3, lst_ints), (1, lst_ints_rev)).foreach { i_xs => 
                i_xs match { case (i, xs) =>
            assertResult(i) { Sequenceops_java.indexOf_lp[Integer](el,
                xs.asJava, Util.intCmp) } }
        }
        
        Array[((Integer, Array[Integer], Comparator[Integer]) => Int,
            (Integer, Array[Integer], Comparator[Integer]) => 
            Option[Integer])]((SeqopsArr.indexOf_r, SeqopsArr.find_r),
                (SeqopsArr.indexOf_i, SeqopsArr.find_i)).foreach {
                    fnI_fnF => fnI_fnF match { case (fnI, fnF) =>
            assertResult(3) { fnI(el, ints, Util.intCmp) }
            assertResult(1) { fnI(el, ints_rev, Util.intCmp) }
            assertResult(Some(el)) { fnF(el, ints, Util.intCmp) }
            assertResult(Some(el)) { fnF(el, ints_rev, Util.intCmp) } }
        }
        List[((Integer, List[Integer], Comparator[Integer]) => Int,
            (Integer, List[Integer], Comparator[Integer]) => 
            Option[Integer])]((Seqops.indexOf_r, Seqops.find_r),
                (Seqops.indexOf_i, Seqops.find_i)).foreach {
                    fnI_fnF => fnI_fnF match { case (fnI, fnF) =>
            assertResult(3) { fnI(el, lst_ints, Util.intCmp) }
            assertResult(1) { fnI(el, lst_ints_rev, Util.intCmp) }
            assertResult(Some(el)) { fnF(el, lst_ints, Util.intCmp) }
            assertResult(Some(el)) { fnF(el, lst_ints_rev, Util.intCmp) } }
        }
	}
    
    it should "find min|max" taggedAs(Tag2) in {
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Array[Integer] => Integer, Array[Integer] => Integer)](
                    (SeqopsArr.min_i[Integer], SeqopsArr.max_i[Integer]),
                    (SeqopsArr.min_r[Integer], SeqopsArr.max_r[Integer])).
                        foreach { fnMin_fnMax => fnMin_fnMax match {
                        case (fnMin, fnMax) =>
                assertResult(arr.min) { fnMin(arr) }
                assertResult(arr.max) { fnMax(arr) } }
            }
			List[(List[Integer] => Integer, List[Integer] => Integer)](
                    (Seqops.min_i[Integer], Seqops.max_i[Integer]),
                    (Seqops.min_r[Integer], Seqops.max_r[Integer])).foreach {
                        fnMin_fnMax => fnMin_fnMax match {
                        case (fnMin, fnMax) =>
                assertResult(lst.min) { fnMin(lst) }
                assertResult(lst.max) { fnMax(lst) } }
            }
		}}
    }
    
    it should "reverse sequence" taggedAs(Tag1) in {
        val jtmpA = ints.map(identity)
        Sequenceops_java.reverse_lp[Integer](jtmpA)
        assertResult(ints_rev) { jtmpA }
        val jtmpL = lst_ints.map(identity).toBuffer
        Sequenceops_java.reverse_lp[Integer](jtmpL.asJava)
        assertResult(lst_ints_rev) { jtmpL }
        Array[Array[Integer] => Unit](SeqopsArr.reverse_mut_lp, 
                SeqopsArr.reverse_mut_i).foreach { f =>
            val tmp = ints.map(identity)
        	f(tmp)
        	assertResult(ints_rev) { tmp } }
        Array[Array[Integer] => Array[Integer]](SeqopsArr.reverse_r, 
                SeqopsArr.reverse_i).foreach { f =>
            assertResult(ints_rev) { f(ints) } }
        
        List[Buffer[Integer] => Unit](Seqops.reverse_mut_lp, 
                Seqops.reverse_mut_i).foreach { f =>
            val tmp = lst_ints.map(identity).toBuffer
        	f(tmp)
        	assertResult(lst_ints_rev) { tmp } }
        List[List[Integer] => List[Integer]](Seqops.reverse_r, 
                Seqops.reverse_i).foreach { f =>
            assertResult(lst_ints_rev) { f(lst_ints) } }
	}
    
    it should "copy sequence" in {
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Array[Integer]) => Array[Integer]](SeqopsArr.copyOf_i,
                    SeqopsArr.copyOf_r).foreach { f => 
                assertResult(true) { arr sameElements f(arr) } }
			List[List[Integer] => List[Integer]](Seqops.copyOf_i,
                    Seqops.copyOf_r).foreach { f => 
                assertResult(lst) { f(lst) } }
        }}
    }
    
    it should "take|drop n items" in {
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[((Int, Array[Integer]) => Array[Integer], 
                (Int, Array[Integer]) => Array[Integer])](
                    (SeqopsArr.take_i[Integer], SeqopsArr.drop_i[Integer])).
                        foreach { fnTake_fnDrop => fnTake_fnDrop match {
                        case (fnTake, fnDrop) =>
                assertResult(true) { arr.take(3) sameElements fnTake(3, arr) }
                assertResult(true) { arr.drop(3) sameElements fnDrop(3, arr) } }
			}
			List[((Int, List[Integer]) => List[Integer],
                (Int, List[Integer]) => List[Integer])](
                    (Seqops.take_i[Integer], Seqops.drop_i[Integer])).
                        foreach { fnTake_fnDrop => fnTake_fnDrop match {
                        case (fnTake, fnDrop) =>
                assertResult(lst.take(3)) { fnTake(3, lst) }
                assertResult(lst.drop(3)) { fnDrop(3, lst) } }
			}
		}}
    }
    
    it should "condition exists|forall items" in {
        val (boolOpA1, boolOpA2) = (((e: Int) => 0 == e % 2), 
			((a: Array[Int]) => !a.isEmpty))
		val (arr1, arr3) = (Array[Int](1, 2, 3), Array[Int](6, 2, 4))
        val arr2 = Array(Array[Int](1, 2), Array[Int](), Array[Int](3, 4))
		val arr4 = Array(Array[Int](1, 2), Array[Int](5), Array[Int](3, 4))
		Array[(((Int => Boolean), Array[Int]) => Boolean, 
            ((Int => Boolean), Array[Int]) => Boolean)](
                (SeqopsArr.exists_i, SeqopsArr.forall_i),
                (SeqopsArr.exists_r, SeqopsArr.forall_r)).foreach { 
                    fnExists_fnForall => fnExists_fnForall match {
                case (fnExists, fnForall) =>
            assertResult(arr1.exists(boolOpA1)) { fnExists(boolOpA1, arr1) }
            assertResult(arr3.exists(boolOpA1)) { fnExists(boolOpA1, arr3) }
        }}
        Array[(((Array[Int] => Boolean), Array[Array[Int]]) => Boolean, 
            ((Array[Int] => Boolean), Array[Array[Int]]) => Boolean)](
                (SeqopsArr.exists_i, SeqopsArr.forall_i),
                (SeqopsArr.exists_r, SeqopsArr.forall_r)).foreach {
                    fnExists_fnForall => fnExists_fnForall match {
                    case (fnExists, fnForall) =>
            assertResult(arr2.forall(boolOpA2)) { fnForall(boolOpA2, arr2) }
            assertResult(arr4.forall(boolOpA2)) { fnForall(boolOpA2, arr4) }
        }}
        
        val (boolOp1, boolOp2) = (((e: Int) => 0 == e % 2), 
			((l: List[Int]) => !l.isEmpty))
        val (lst1, lst3) = (List[Int](1, 2, 3), List[Int](6, 2, 4))
        val lst2 = List(List[Int](1, 2), List[Int](), List[Int](3, 4))
		val lst4 = List(List[Int](1, 2), List[Int](5), List[Int](3, 4))
        List[(((Int => Boolean), List[Int]) => Boolean, 
            ((Int => Boolean), List[Int]) => Boolean)](
                (Seqops.exists_i, Seqops.forall_i),
                (Seqops.exists_r, Seqops.forall_r)).foreach { 
                    fnExists_fnForall => fnExists_fnForall match {
                    case (fnExists, fnForall) =>
            assertResult(lst1.exists(boolOp1)) { fnExists(boolOp1, lst1) }
            assertResult(lst3.exists(boolOp1)) { fnExists(boolOp1, lst3) }
        }}
        List[(((List[Int] => Boolean), List[List[Int]]) => Boolean, 
            ((List[Int] => Boolean), List[List[Int]]) => Boolean)](
                (Seqops.exists_i, Seqops.forall_i),
                (Seqops.exists_r, Seqops.forall_r)).foreach {
                    fnExists_fnForall => fnExists_fnForall match {
                    case (fnExists, fnForall) =>
            assertResult(lst2.forall(boolOp2)) { fnForall(boolOp2, lst2) }
            assertResult(lst4.forall(boolOp2)) { fnForall(boolOp2, lst4) }
        }}
    }
    
    it should "map proc on elems" in {
        val proc1 = ((n: Integer) => n + 2 : Integer)
		Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[((Integer => Integer), Array[Integer]) => Array[Integer]](
                    SeqopsArr.map_i, SeqopsArr.map_r).foreach { f =>
				assertResult(true) { arr.map(proc1) sameElements f(proc1, arr) } }
			List[((Integer => Integer), List[Integer]) => List[Integer]](
                    Seqops.map_i, Seqops.map_r).foreach { f =>
				assertResult(lst.map(proc1)) { f(proc1, lst) } }
		}}
    }
    
    it should "foreach elem" in {
        val proc1 = ((n: Integer) => printf("%s ", n.toString))
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[((Integer => Unit), Array[Integer]) => Unit](
                    SeqopsArr.foreach_i, SeqopsArr.foreach_r).foreach { f =>
				assertResult(arr.foreach(proc1)) { f(proc1, arr) } }
			List[((Integer => Unit), List[Integer]) => Unit](Seqops.foreach_i,
                    Seqops.foreach_r).foreach { f => 
				assertResult(lst.foreach(proc1)) { f(proc1, lst) } }
		}}
    } 
    
    it should "filter|remove elems" in {
        val boolOp1 = ((e: Integer) => 0 == (e % 2))
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(((Integer => Boolean), Array[Integer]) => Array[Integer], 
                ((Integer => Boolean), Array[Integer]) => Array[Integer])](
                    (SeqopsArr.filter_i[Integer], SeqopsArr.remove_i[Integer]),
                    (SeqopsArr.filter_r[Integer], SeqopsArr.remove_r[Integer])).foreach { 
                        fnFilter_fnRemove => fnFilter_fnRemove match {
                        case (fnFilter, fnRemove) =>
                assertResult(true) {
                    arr.filter(boolOp1) sameElements fnFilter(boolOp1, arr) }
                assertResult(true) {
                    arr.filterNot(boolOp1) sameElements fnRemove(boolOp1, arr) } }
            }
			List[(((Integer => Boolean), List[Integer]) => List[Integer], 
				((Integer => Boolean), List[Integer]) => List[Integer])](
                    (Seqops.filter_i, Seqops.remove_i),
                    (Seqops.filter_r, Seqops.remove_r)).foreach { 
                        fnFilter_fnRemove => fnFilter_fnRemove match {
                        case (fnFilter, fnRemove) =>
                assertResult(lst.filter(boolOp1)) { fnFilter(boolOp1, lst) }
                assertResult(lst.filterNot(boolOp1)) { fnRemove(boolOp1, lst) } }
            }
		}}
    }
    
    it should "fold left over sequence" in {
		val corp1 = ((a: Int, e: Integer) => a + e)
		val corp2 = ((a: Int, e: Integer) => a - e)
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Int, ((Int, Integer) => Int), Array[Integer]) => Int](
                    SeqopsArr.foldLeft_i, SeqopsArr.foldLeft_r).foreach { f =>
				assertResult(arr.foldLeft(0)(corp1)) { f(0, corp1, arr) }
				assertResult(arr.foldLeft(0)(corp2)) { f(0, corp2, arr) } }
			List[(Int, ((Int, Integer) => Int), List[Integer]) => Int](
                    Seqops.foldLeft_i, Seqops.foldLeft_r).foreach { f =>
				assertResult(lst.foldLeft(0)(corp1)) { f(0, corp1, lst) }
				assertResult(lst.foldLeft(0)(corp2)) { f(0, corp2, lst) } }
		}}
    } 
    
    it should "fold right over sequence" in {
		val proc1 = ((e: Integer, a: Int) => e + a)
		val proc2 = ((e: Integer, a: Int) => e - a)
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Int, ((Integer, Int) => Int), Array[Integer]) => Int](
                    SeqopsArr.foldRight_i, SeqopsArr.foldRight_r).foreach { f =>
				assertResult(arr.foldRight(0)(proc1)) { f(0, proc1, arr) }
				assertResult(arr.foldRight(0)(proc2)) { f(0, proc2, arr) } }
			List[(Int, ((Integer, Int) => Int), List[Integer]) => Int](
                    Seqops.foldRight_i, Seqops.foldRight_r).foreach { f =>
				assertResult(lst.foldRight(0)(proc1)) { f(0, proc1, lst) }
				assertResult(lst.foldRight(0)(proc2)) { f(0, proc2, lst) } }
		}}
    } 
    
    it should "unfold right" in {
		val func1 = ((h_t:(Int, Int)) => h_t match {
			case (h, t) => 0 == t match { 
				case true => None
				case _ => Some(h, (h + 1, t - h))}})
		val func2 = ((h_t:(Int, Int)) => h_t match {
			case (h, t) => 0 == t match { 
				case true => None
				case _ => Some(h, (h + 1, h - t))}})
		val (seed1, seed2) = ((0, 10), (0, 2))
		Array[((((Int, Int)) => Option[(Int, (Int, Int))]), (Int, Int)) =>
                Array[Int]](SeqopsArr.unfoldRight_i).foreach { f =>
			assertResult(true) {
                Array.range(4, -1, -1) sameElements f(func1, seed1) }
			assertResult(true) {
                Array.range(4, -1, -1) sameElements f(func2, seed2) } }
		List[((((Int, Int)) => Option[(Int, (Int, Int))]), (Int, Int)) =>
                List[Int]](Seqops.unfoldRight_i).foreach { f =>
			assertResult(List.range(4, -1, -1)) { f(func1, seed1) }
			assertResult(List.range(4, -1, -1)) { f(func2, seed2) } }
    } 
    
    it should "unfold left" in {
		val func1 = ((h_t:(Int, Int)) => h_t match {
			case (h, t) => 0 == t match { 
				case true => None
				case _ => Some(h, (h + 1, t - h))}})
		val func2 = ((h_t:(Int, Int)) => h_t match {
			case (h, t) => 0 == t match { 
				case true => None
				case _ => Some(h, (h + 1, h + t))}})
		val (seed1, seed2) = ((0, 10), (0, -10))
		Array[((((Int, Int)) => Option[(Int, (Int, Int))]), (Int, Int)) =>
                Array[Int]](SeqopsArr.unfoldLeft_r).foreach { f =>
			assertResult(true) {
                Array.range(0, 5) sameElements f(func1, seed1) }
			assertResult(true) {
                Array.range(0, 5) sameElements f(func2, seed2) } }
		List[((((Int, Int)) => Option[(Int, (Int, Int))]), (Int, Int)) =>
                List[Int]](Seqops.unfoldLeft_r).foreach { f =>
			assertResult(List.range(0, 5)) { f(func1, seed1) }
			assertResult(List.range(0, 5)) { f(func2, seed2) } }
    } 
    
    it should "sort sequence" in {
		def verifyfn[T](cmpfn: ((T, T) => Boolean), coll: Iterable[T]): 
				Boolean = coll.toList match {
			case Nil => true
			case x :: xs => xs.foldLeft((true, x))((a_cur, e) => a_cur match {
					case (a, cur) => (cmpfn(cur, e) && a, e) })._1
		}
		val ints1 = Array[Integer](9, 9, 9, 0, 3, 4)
		val ints2 = Array[Integer](4, 0, 9, 9, 9, 3)
        Array[(Array[Integer], Boolean) => Boolean](SeqopsArr.isOrdered_i,
                SeqopsArr.isOrdered_r).foreach { f =>
			assertResult(verifyfn[Integer]((_ <= _), ints)) { f(ints, false) }
			assertResult(verifyfn[Integer]((_ >= _), ints_rev)) { 
				f(ints_rev, true) }
			
			val (tmp1, tmp2) = (ints1.map(identity), ints2.map(identity))
			assertResult(false) { f(tmp1, false) || f(tmp2, false) }
			SeqopsArr.quickSort_lp[Integer](tmp1, 0, tmp1.size - 1)
			SeqopsArr.quickSort_lp[Integer](tmp2, 0, tmp2.size - 1)
			assertResult(true) { f(tmp1, false) && f(tmp2, false) } }
        
		val lst_ints1 = Buffer[Integer](9, 9, 9, 0, 3, 4)
		val lst_ints2 = Buffer[Integer](4, 0, 9, 9, 9, 3)
		List[(Iterable[Integer], Boolean) => Boolean](Seqops.isOrdered_i,
                Seqops.isOrdered_r).foreach { f =>
			assertResult(verifyfn[Integer]((_ <= _), lst_ints)) {
				f(lst_ints, false) }
			assertResult(verifyfn[Integer]((_ >= _), lst_ints_rev)) { 
				f(lst_ints_rev, true) }
			
			val (tmp1, tmp2) = (lst_ints1.map(identity), 
				lst_ints2.map(identity))
			assertResult(false) { f(tmp1, false) || f(tmp2, false) }
			Seqops.quickSort_lp[Integer](tmp1, 0, tmp1.size - 1)
			Seqops.quickSort_lp[Integer](tmp2, 0, tmp2.size - 1)
			assertResult(true) { f(tmp1, false) && f(tmp2, false) } }
    } 
    
    it should "append sequence" in {
        val (lst_nines, nines) = (List[Integer](9, 9, 9, 9),
            Array[Integer](9, 9, 9, 9))
		Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Array[Integer], Array[Integer]) => Array[Integer]](
                    SeqopsArr.append_i, SeqopsArr.append_r).foreach { f =>
				assertResult(true) {
                    (arr ++ nines) sameElements f(arr, nines) } }
			List[(List[Integer], List[Integer]) => List[Integer]](
                    Seqops.append_i, Seqops.append_r).foreach { f => 
				assertResult(lst ++ lst_nines) { f(lst, lst_nines) } }
		}}
    }
    
    it should "interleave sequences" in {
		val nines = Array[Integer](9, 9, 9, 9)
		val ansA = Array[Integer](0, 9, 1, 9, 2, 9, 3, 9, 4)
		Array[(Array[Integer], Array[Integer]) => Array[Integer]](
                SeqopsArr.interleave_i, SeqopsArr.interleave_r).foreach { f =>
			assertResult(true) { ansA sameElements f(ints, nines) } }
		
        val lst_nines = List[Integer](9, 9, 9, 9)
		val ansL = List[Integer](0, 9, 1, 9, 2, 9, 3, 9, 4)
		List[(List[Integer], List[Integer]) => List[Integer]](
                Seqops.interleave_i, Seqops.interleave_r).foreach { f =>
			assertResult(ansL) { f(lst_ints, lst_nines) } }
    } 
    
    it should "map proc over 2 sequences" in {
        val proc1 = (e1: Integer, e2: Integer) => (e1 + e2) + 2 : Integer
		Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[((Integer, Integer) => Integer, Array[Integer],
                Array[Integer]) => Array[Integer]](SeqopsArr.map2_i, 
                    SeqopsArr.map2_r).foreach { f =>
                assertResult(true) { arr.zip(arr).map((e1_e2 => 
                    e1_e2._1 + e1_e2._2 + 2)) sameElements
                    f(proc1, arr, arr) } }
			List[((Integer, Integer) => Integer, List[Integer],
                List[Integer]) => List[Integer]](Seqops.map2_i,
                    Seqops.map2_r).foreach { f =>
                assertResult(lst.zip(lst).map((e1_e2 => 
                    e1_e2._1 + e1_e2._2 + 2))) { f(proc1, lst, lst) } }
		}}
    }
    
    it should "zip 2 sequences" in {
        Array((lst_ints, ints), (lst_ints_rev, ints_rev)).foreach { lst_arr =>
				lst_arr match { case (lst, arr) =>
			Array[(Array[Integer], Array[Integer]) => Array[(Integer, Integer)]](
                    SeqopsArr.zip_i, SeqopsArr.zip_r).foreach { f => 
				assertResult(true) { arr.zip(arr) sameElements f(arr, arr) } }
			List[(List[Integer], List[Integer]) => List[(Integer, Integer)]](
                    Seqops.zip_i, Seqops.zip_r).foreach { f => 
                assertResult(lst.zip(lst)) { f(lst, lst) } }
		}}
    } 
    
    it should "unzip sequence" in {
        val nlst1 = lst_ints.zip(lst_ints_rev)
		val arrs1 = ints.zip(ints_rev)
		Array[Array[(Integer, Integer)] => (Array[Integer], Array[Integer])](
				SeqopsArr.unzip_i).foreach { f => 
			assertResult(true) { arrs1.unzip._1 sameElements f(arrs1)._1 }
			assertResult(true) { arrs1.unzip._2 sameElements f(arrs1)._2 } }
		List[List[(Integer, Integer)] => (List[Integer], List[Integer])](
				Seqops.unzip_i).foreach { f => 
			assertResult(nlst1.unzip._1) { f(nlst1)._1 }
			assertResult(nlst1.unzip._2) { f(nlst1)._2 } }
    } 
    
    it should "concat(flatten) nested sequences" in {
        val nlst1 = List(lst_ints, lst_ints_rev)
		val arrs1 = Array(ints, ints_rev)
		Array[Array[Array[Integer]] => Array[Integer]](SeqopsArr.concat_i,
                SeqopsArr.concat_r).foreach { f => 
			assertResult(true) {
                Array.concat(arrs1: _*) sameElements f(arrs1) } }
		List[List[List[Integer]] => List[Integer]](Seqops.concat_i,
                Seqops.concat_r).foreach { f => 
			assertResult(List.concat(nlst1: _*)) { 
				f(List(lst_ints, lst_ints_rev)) } }
    } 
}

object SequenceopsTest {
    
}

}

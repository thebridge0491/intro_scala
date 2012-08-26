package org.sandbox.intro_scala.practice {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._
import java.util.Comparator
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import org.sandbox.intro_scala.util.{Library => Util}
import org.sandbox.intro_scala.practice.{Sequenceops => Seqops, 
	SequenceopsArray => SeqopsArr}

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
		elems <- Gen.containerOfN[List, Integer](numElems, gen0)
	} yield elems
    
    
    // scalacheck-style property define
    property("tabulate sequence") = forAll(Gen.choose(0, 18)) { n =>
        val func1 = (e: Int) => e + 2
        val ansA = Array.range(0, n).foldLeft(Array[Int]())(
            (a, e) => func1(e) +: a).reverse
        val funcsA = Array[(Int => Int, Int) => Array[Int]](
            SeqopsArr.tabulate_i, SeqopsArr.tabulate_r)
        val ansL = List.range(0, n).foldLeft(List[Int]())(
            (a, e) => func1(e) :: a).reverse
        val funcsL = List[(Int => Int, Int) => List[Int]](
            Seqops.tabulate_i, Seqops.tabulate_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(func1, n)) }).
            label("===propTabulateA(%d) : %s===".format(n,
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && (ansL == f(func1, n)) }).
            label("===propTabulateL(%d) : %s===".format(n,
            ansL.mkString("[", ", ", "]")))
    }
    
    property("compute length") = forAll(genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer]) =>
        val arr = xs.toArray
        val funcsA = Array[Array[Integer] => Int](SeqopsArr.length_i,
            SeqopsArr.length_r)
        val funcsL = List[List[Integer] => Int](Seqops.length_i,
            Seqops.length_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && (arr.size == f(arr)) }).
            label("===propLengthA(%s) : %d===".format(
            arr.mkString("[", ", ", "]"), arr.size)) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && (xs.size == f(xs)) }).
            label("===propLengthL(%s) : %d===".format(
            xs.mkString("[", ", ", "]"), xs.size))
    }
    
    property("access nth item") = forAll(genListInts(Gen.choose(0, 100))) { 
            (xs: List[Integer]) =>
        val (n, arr) = (scala.util.Random.nextInt(xs.size), xs.toArray)
        val funcsA = Array[(Int, Array[Integer]) => Option[Integer]](
            SeqopsArr.nth_i, SeqopsArr.nth_r)
        val funcsL = List[(Int, List[Integer]) => Option[Integer]](
            Seqops.nth_i, Seqops.nth_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (Some(arr(n)) == f(n, arr)) }).
            label("===propNthA(%d, %s) : %d===".format(n,
            arr.mkString("[", ", ", "]"), arr(n))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (Some(xs(n)) == f(n, xs)) }).
            label("===propNthL(%d, %s) : %d===".format(n,
            xs.mkString("[", ", ", "]"), xs(n)))
    }
    
    property("find index|item") = forAll(genListInts(Gen.choose(0, 100)),
            Gen.choose[Integer](0, 100)) {
            (xs: List[Integer], el: Integer) => 
        val arr = xs.toArray
        val (ansIA, ansFA) = (arr.indexOf(el), arr.find(e => e == el))
        val (ansIL, ansFL) = (xs.indexOf(el), xs.find(e => e == el))
        val funcsA = Array[((Integer, Array[Integer], Comparator[Integer]) =>
                Int, (Integer, Array[Integer], Comparator[Integer]) => 
                Option[Integer])]((SeqopsArr.indexOf_r, SeqopsArr.find_r),
            (SeqopsArr.indexOf_i, SeqopsArr.find_i))
        val funcsL = List[((Integer, List[Integer], Comparator[Integer]) =>
                Int, (Integer, List[Integer], Comparator[Integer]) => 
                Option[Integer])]((Seqops.indexOf_r, Seqops.find_r),
            (Seqops.indexOf_i, Seqops.find_i))
        
        (ansIA == Sequenceops_java.indexOf_lp[Integer](el, arr,
            Util.intCmp)).label(
            "===(java) propIndexOfA(%d) : %d===".format(el, ansIA)) &&
        (ansIL == Sequenceops_java.indexOf_lp[Integer](el, xs.asJava, 
            Util.intCmp)).label(
            "===(java) propIndexOfL(%d) : %d===".format(el, ansIL)) &&
        (funcsA.foldLeft(true) { (acc, fnI_fnF) => (acc, fnI_fnF) match { 
                case (acc, (fnI, fnF)) => acc && 
            (ansIA == fnI(el, arr, Util.intCmp) && 
            ansFA == fnF(el, arr, Util.intCmp)) }}).label(
            "===propIndexFindA(%d) : %d %s===".format(el, ansIA, ansFA)) &&
        (funcsL.foldLeft(true) { (acc, fnI_fnF) => (acc, fnI_fnF) match { 
                case (acc, (fnI, fnF)) => acc && 
            (ansIL == fnI(el, xs, Util.intCmp) && 
            ansFL == fnF(el, xs, Util.intCmp)) }}).label(
            "===propIndexFindL(%d) : %d %s===".format(el, ansIL, ansFL))
	}
    
    property("find min|max") = forAll(genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer]) =>
        val arr = xs.toArray
        val (ansMinA, ansMaxA) = (arr.min, arr.max)
        val (ansMinL, ansMaxL) = (xs.min, xs.max)
        val funcsA = Array[(Array[Integer] => Integer, Array[Integer] => 
                Integer)](
            (SeqopsArr.min_i[Integer], SeqopsArr.max_i[Integer]),
            (SeqopsArr.min_r[Integer], SeqopsArr.max_r[Integer]))
        val funcsL = List[(List[Integer] => Integer, List[Integer] => 
                Integer)](
            (Seqops.min_i[Integer], Seqops.max_i[Integer]),
            (Seqops.min_r[Integer], Seqops.max_r[Integer]))
        
        (funcsA.foldLeft(true) { (acc, fnMin_fnMax) => 
                (acc, fnMin_fnMax) match { case (acc, (fnMin, fnMax)) =>
            acc && (ansMinA == fnMin(arr) && ansMaxA == fnMax(arr)) }}).label(
            "===propMinMaxA(%s) : %d %d===".format(
            arr.mkString("[", ", ", "]"), ansMinA, ansMaxA)) &&
        (funcsL.foldLeft(true) { (acc, fnMin_fnMax) => 
                (acc, fnMin_fnMax) match { case (acc, (fnMin, fnMax)) =>
            acc && (ansMinL == fnMin(xs) && ansMaxL == fnMax(xs)) }}).
            label("===propMinMaxL(%s) : %d %d===".format(
            xs.mkString("[", ", ", "]"), ansMinL, ansMaxL))
    }
    
    property("reverse sequence") = forAll(genListInts(Gen.choose(0, 100))) { 
            (xs: List[Integer]) => 
        val arr = xs.toArray
        var mutCopyA: Array[Integer] = null //arr.map(identity)
        var mutCopyL: Buffer[Integer] = null //xs.map(identity)
        val (ansA, ansL) = (arr.map(identity).reverse, 
            xs.map(identity).reverse)
        val funcsMutA = Array[(Array[Integer]) => Unit](
            SeqopsArr.reverse_mut_lp, SeqopsArr.reverse_mut_i)
        val funcsMutL = List[(Buffer[Integer]) => Unit](
            Seqops.reverse_mut_lp, Seqops.reverse_mut_i)
        val funcsImmA = Array[Array[Integer] => Array[Integer]](
            SeqopsArr.reverse_r, SeqopsArr.reverse_i)
        val funcsImmL = List[List[Integer] => List[Integer]](
            Seqops.reverse_r, Seqops.reverse_i)
        
        ({ mutCopyA = arr.map(identity) ; 
            Sequenceops_java.reverse_lp[Integer](mutCopyA) ;
            ansA sameElements mutCopyA }).label(
            "===(java) mut. propReverseA(%s) : %s  %s===".format(
            arr.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"), 
            mutCopyA.mkString("[", ", ", "]"))) &&
        ({ mutCopyL = xs.map(identity).toBuffer ;
            Sequenceops_java.reverse_lp[Integer](mutCopyL.asJava) ;
            ansL == mutCopyL }).label(
            "===(java) mut. propReverseL(%s) : %s  %s===".format(
            xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]"), 
            mutCopyL.mkString("[", ", ", "]"))) &&
        (funcsMutA.foldLeft(true) { (acc, f) => 
            mutCopyA = arr.map(identity) ; f(mutCopyA) ; 
            acc && (ansA sameElements mutCopyA) }).label(
            "===mut. propReverseA(%s) : %s  %s===".format(
            arr.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"), 
            mutCopyA.mkString("[", ", ", "]"))) &&
        (funcsMutL.foldLeft(true) { (acc, f) => 
            mutCopyL = xs.map(identity).toBuffer ; f(mutCopyL) ; acc && 
            (ansL == mutCopyL) }).label(
            "===mut. propReverseL(%s) : %s  %s===".format(
            xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]"), 
            mutCopyL.mkString("[", ", ", "]"))) &&
        (funcsImmA.foldLeft(true) { (acc, f) => 
            acc && (ansA sameElements f(arr)) }).label(
            "===immut. propReverseA(%s) : %s===".format(
            arr.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]"))) &&
        (funcsImmL.foldLeft(true) { (acc, f) => 
            acc && (ansL == f(xs)) }).label(
            "===immut. propReverseL(%s) : %s===".format(
            xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]")))
	}
    
    property("copy sequence") = forAll(genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer]) =>
        val arr = xs.toArray
        val (ansA, ansL) = (arr.map(identity), xs.map(identity))
        val funcsA = Array[(Array[Integer]) => Array[Integer]](
            SeqopsArr.copyOf_i, SeqopsArr.copyOf_r)
        val funcsL = List[List[Integer] => List[Integer]](Seqops.copyOf_i,
            Seqops.copyOf_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(arr)) }).
            label("===propCopyA(%s) : %s===".format(
            arr.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && (ansL == f(xs)) }).
            label("===propCopyL(%s) : %s===".format(
            xs.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]")))
    }
    
    property("take|drop n items") = forAll(genListInts(Gen.choose(0, 100)),
            Gen.choose(0, 20)) { (xs: List[Integer], n: Int) =>
        val arr = xs.toArray
        val (ansTA, ansDA) = (arr.take(n), arr.drop(n))
        val (ansTL, ansDL) = (xs.take(n), xs.drop(n))
        val funcsA = Array[((Int, Array[Integer]) => Array[Integer], 
                (Int, Array[Integer]) => Array[Integer])](
            (SeqopsArr.take_i[Integer], SeqopsArr.drop_i[Integer]))
        val funcsL = List[((Int, List[Integer]) => List[Integer],
                (Int, List[Integer]) => List[Integer])](
            (Seqops.take_i[Integer], Seqops.drop_i[Integer]))
        
        (funcsA.foldLeft(true) { (acc, fnTake_fnDrop) => 
                (acc, fnTake_fnDrop) match { case (acc, (fnTake, fnDrop)) =>
            acc && (ansTA sameElements fnTake(n, arr)) &&
            (ansDA sameElements fnDrop(n, arr)) }}).label(
            "===propTakeDropA(%d, %s) : %s %s===".format(n,
            arr.mkString("[", ", ", "]"), ansTA.mkString("[", ", ", "]"),
            ansDA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, fnTake_fnDrop) => 
                (acc, fnTake_fnDrop) match { case (acc, (fnTake, fnDrop)) =>
            acc && (ansTL == fnTake(n, xs)) &&
            (ansDL == fnDrop(n, xs)) }}).label(
            "===propTakeDropL(%d, %s) : %s %s===".format(n,
            xs.mkString("[", ", ", "]"), ansTL.mkString("[", ", ", "]"),
            ansDL.mkString("[", ", ", "]")))
    }
    
    property("condition exists|forall items") = forAll(
            genListInts(Gen.choose(0, 100))) { (xs: List[Integer]) =>
        val (boolOp, arr) = (((e: Integer) => 0 == e % 2), xs.toArray)
        val (ansEA, ansFA) = (arr.exists(boolOp), arr.forall(boolOp))
        val (ansEL, ansFL) = (xs.exists(boolOp), xs.forall(boolOp))
        val funcsA = Array[(((Integer => Boolean), Array[Integer]) => 
                Boolean, ((Integer => Boolean), Array[Integer]) => Boolean)](
            (SeqopsArr.exists_i, SeqopsArr.forall_i),
            (SeqopsArr.exists_r, SeqopsArr.forall_r))
        val funcsL = List[(((Integer => Boolean), List[Integer]) => Boolean, 
                ((Integer => Boolean), List[Integer]) => Boolean)](
            (Seqops.exists_i, Seqops.forall_i),
            (Seqops.exists_r, Seqops.forall_r))
        
        (funcsA.foldLeft(true) { (acc, fnExists_fnForall) => 
                (acc, fnExists_fnForall) match { 
                case (acc, (fnExists, fnForall)) =>
            acc && (ansEA == fnExists(boolOp, arr)) &&
            (ansFA == fnForall(boolOp, arr)) }}).label(
            "===propExistsForallA(%s) : %s %s===".format(
            arr.mkString("[", ", ", "]"), ansEA, ansFA)) &&
        (funcsL.foldLeft(true) { (acc, fnExists_fnForall) => 
                (acc, fnExists_fnForall) match { 
                case (acc, (fnExists, fnForall)) =>
            acc && (ansEL == fnExists(boolOp, xs)) &&
            (ansFL == fnForall(boolOp, xs)) }}).label(
            "===propExistsForallL(%s) : %s %s===".format(
            xs.mkString("[", ", ", "]"), ansEL, ansFL))
    }
    
    property("map proc on elems") = forAll(genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer]) =>
        val (proc1, arr) = (((n: Integer) => n + 2 : Integer), xs.toArray)
        val (ansA, ansL) = (arr.map(proc1), xs.map(proc1))
        val funcsA = Array[((Integer => Integer), Array[Integer]) => 
                Array[Integer]](SeqopsArr.map_i, SeqopsArr.map_r)
        val funcsL = List[((Integer => Integer), List[Integer]) => 
                List[Integer]](Seqops.map_i, Seqops.map_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(proc1, arr)) }).label(
            "===propMapA(%s) : %s===".format(arr.mkString("[", ", ", "]"), 
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(proc1, xs)) }).label(
            "===propMapL(%s) : %s===".format(xs.mkString("[", ", ", "]"),
            ansL.mkString("[", ", ", "]")))
    }
    
    property("foreach elems") = forAll(genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer]) =>
        val proc1: Integer => Unit = (n => Console.err.printf("%s ",
            n.toString))
        val arr = xs.toArray
        val (ansA, ansL) = (arr.foreach(proc1), xs.foreach(proc1))
        val funcsA = Array[((Integer => Unit), Array[Integer]) => Unit](
            SeqopsArr.foreach_i, SeqopsArr.foreach_r)
        val funcsL = List[((Integer => Unit), List[Integer]) => Unit](
            Seqops.foreach_i, Seqops.foreach_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA.getClass == f(proc1, arr).getClass) }).label(
            "===propMapA(%s) : %s===".format(arr.mkString("[", ", ", "]"), 
            ansA)) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL.getClass == f(proc1, xs).getClass) }).label(
            "===propMapL(%s) : %s===".format(xs.mkString("[", ", ", "]"),
            ansL))
    }
    
    property("filter|remove items") = forAll(
            genListInts(Gen.choose(0, 100))) { (xs: List[Integer]) =>
        val (boolOp, arr) = (((e: Integer) => 0 == e % 2), xs.toArray)
        val (ansFA, ansNA) = (arr.filter(boolOp), arr.filterNot(boolOp))
        val (ansFL, ansNL) = (xs.filter(boolOp), xs.filterNot(boolOp))
        val funcsA = Array[(((Integer => Boolean), Array[Integer]) => 
                Array[Integer], ((Integer => Boolean), Array[Integer]) => 
                Array[Integer])](
            (SeqopsArr.filter_i[Integer], SeqopsArr.remove_i[Integer]),
            (SeqopsArr.filter_r[Integer], SeqopsArr.remove_r[Integer]))
        val funcsL = List[(((Integer => Boolean), List[Integer]) => 
                List[Integer], ((Integer => Boolean), List[Integer]) => 
                List[Integer])](
            (Seqops.filter_i, Seqops.remove_i),
            (Seqops.filter_r, Seqops.remove_r))
        
        (funcsA.foldLeft(true) { (acc, fnFilter_fnRemove) => 
                (acc, fnFilter_fnRemove) match { 
                case (acc, (fnFilter, fnRemove)) =>
            acc && (ansFA sameElements fnFilter(boolOp, arr)) &&
            (ansNA sameElements fnRemove(boolOp, arr)) }}).label(
            "===propFilterRemoveA(%s) : %s %s===".format(
            arr.mkString("[", ", ", "]"), ansFA.mkString("[", ", ", "]"),
            ansNA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, fnFilter_fnRemove) => 
                (acc, fnFilter_fnRemove) match { 
                case (acc, (fnFilter, fnRemove)) =>
            acc && (ansFL == fnFilter(boolOp, xs)) &&
            (ansNL == fnRemove(boolOp, xs)) }}).label(
            "===propFilterRemoveL(%s) : %s %s===".format(
            xs.mkString("[", ", ", "]"), ansFL.mkString("[", ", ", "]"),
            ansNL.mkString("[", ", ", "]")))
    }
    
    property("fold left over sequence") = forAll(genListInts(
            Gen.choose(0, 100))) { (xs: List[Integer]) =>
        val arr = xs.toArray
        val corp1 = ((a: Int, e: Integer) => a + e)
		val corp2 = ((a: Int, e: Integer) => a - e)
        val (ansA1, ansA2) = (arr.foldLeft(0)(corp1), arr.foldLeft(0)(corp2))
        val (ansL1, ansL2) = (xs.foldLeft(0)(corp1), xs.foldLeft(0)(corp2))
        val funcsA = Array[(Int, ((Int, Integer) => Int), Array[Integer]) => 
                Int](SeqopsArr.foldLeft_i, SeqopsArr.foldLeft_r)
        val funcsL = List[(Int, ((Int, Integer) => Int), List[Integer]) => 
                Int](Seqops.foldLeft_i, Seqops.foldLeft_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA1 == f(0, corp1, arr) && ansA2 == f(0, corp2, arr)) }).label(
            "===propFoldLeftA(%s) : %d %d===".format(
            arr.mkString("[", ", ", "]"), ansA1, ansA2)) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL1 == f(0, corp1, xs) && ansL2 == f(0, corp2, xs)) }).label(
            "===propFoldLeftL(%s) : %d %d===".format(
            xs.mkString("[", ", ", "]"), ansL1, ansL2))
    }
    
    property("fold right over sequence") = forAll(genListInts(
            Gen.choose(0, 100))) { (xs: List[Integer]) =>
        val arr = xs.toArray
        val proc1 = ((e: Integer, a: Int) => e + a)
		val proc2 = ((e: Integer, a: Int) => e - a)
        val (ansA1, ansA2) = (arr.foldRight(0)(proc1), arr.foldRight(0)(proc2))
        val (ansL1, ansL2) = (xs.foldRight(0)(proc1), xs.foldRight(0)(proc2))
        val funcsA = Array[(Int, ((Integer, Int) => Int), Array[Integer]) => 
                Int](SeqopsArr.foldRight_i, SeqopsArr.foldRight_r)
        val funcsL = List[(Int, ((Integer, Int) => Int), List[Integer]) => 
                Int](Seqops.foldRight_i, Seqops.foldRight_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA1 == f(0, proc1, arr) && ansA2 == f(0, proc2, arr)) }).label(
            "===propFoldRightA(%s) : %d %d===".format(
            arr.mkString("[", ", ", "]"), ansA1, ansA2)) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL1 == f(0, proc1, xs) && ansL2 == f(0, proc2, xs)) }).label(
            "===propFoldRightL(%s) : %d %d===".format(
            xs.mkString("[", ", ", "]"), ansL1, ansL2))
    }

    property("unfold right") = forAll(Gen.choose(0, 100)) { n =>
        def unfold[T, U](func: (U => Option[(T, U)]), seed: U): List[T] =
                func(seed) match {
            case None => Nil
            case Some((a, new_seed)) => a :: unfold(func, new_seed)
        }
        val funcRg = ((h_t:(Int, Int)) => h_t match {
			case (h, t) => t < h match {
				case true => None
				case _ => Some(h, (h + 1, t))}})
		val funcExpand = ((bs: Int) => (num: Int) => num match {
            case num => 0 >= num match {
                case true => None
                case _ => Some(num % bs, num / bs)}})
		val (seedRg, seedExpand) = ((0, n % 20), n)
        val ansRgL = unfold(funcRg, seedRg).reverse
        val ansExpandL = unfold(funcExpand(2), seedExpand).reverse
        val (ansRgA, ansExpandA) = (ansRgL.toArray, ansExpandL.toArray)
        
        (ansRgA sameElements SeqopsArr.unfoldRight_i(funcRg, seedRg)).
            label("===propUnfoldRightA range %s : %s===".format(seedRg,
            ansRgA.mkString("[", ", ", "]"))) &&
        (ansExpandA sameElements SeqopsArr.unfoldRight_i(funcExpand(2), 
            seedExpand)).label("===propUnfoldRightA expand %s : %s===".format(
            seedExpand, ansExpandA.mkString("[", ", ", "]"))) &&
        (ansRgL == Seqops.unfoldRight_i(funcRg, seedRg)).
            label("===propUnfoldRightL range %s : %s===".format(seedRg,
            ansRgL.mkString("[", ", ", "]"))) &&
        (ansExpandL == Seqops.unfoldRight_i(funcExpand(2), seedExpand)).
            label("===propUnfoldRightL expand %s : %s===".format(seedExpand,
            ansExpandL.mkString("[", ", ", "]")))
    }

    property("unfold left") = forAll(Gen.choose(0, 100)) { n =>
        def unfold[T, U](func: (U => Option[(T, U)]), seed: U): List[T] =
                func(seed) match {
            case None => Nil
            case Some((a, new_seed)) => a :: unfold(func, new_seed)
        }
        val funcFib = ((s0_s1_num:(Int, Int, Int)) => s0_s1_num match {
			case (s0, s1, num) => 0 > num match {
				case true => None
				case _ => Some(s0, (s0 + s1, s0, num - 1))}})
		val funcUnsum = ((h_t:(Int, Int)) => h_t match {
            case (h, t) => t < h match {
                case true => None
                case _ => Some(h, (h + 1, t - h))}})
		val (seedFib, seedUnsum) = ((0, 1, n % 20), (0, n))
        val ansFibL = unfold(funcFib, seedFib)
        val ansUnsumL = unfold(funcUnsum, seedUnsum)
        val (ansFibA, ansUnsumA) = (ansFibL.toArray, ansUnsumL.toArray)
        
        (ansFibA sameElements SeqopsArr.unfoldLeft_r(funcFib, seedFib)).
            label("===propUnfoldLeftA fib %s : %s===".format(seedFib,
            ansFibA.mkString("[", ", ", "]"))) &&
        (ansUnsumA sameElements SeqopsArr.unfoldLeft_r(funcUnsum, 
            seedUnsum)).label("===propUnfoldLeftA unsum %s : %s===".format(
            seedUnsum, ansUnsumA.mkString("[", ", ", "]"))) &&
        (ansFibL == Seqops.unfoldLeft_r(funcFib, seedFib)).
            label("===propUnfoldLeftL fib %s : %s===".format(seedFib,
            ansFibL.mkString("[", ", ", "]"))) &&
        (ansUnsumL == Seqops.unfoldLeft_r(funcUnsum, seedUnsum)).
            label("===propUnfoldLeftL unsum %s : %s===".format(seedUnsum,
            ansUnsumL.mkString("[", ", ", "]")))
    }
    
    property("sort sequence") = forAll(genListInts(Gen.choose(0, 100))) { 
            (ms: List[Integer]) =>
		def verifyfn[T](cmpfn: ((T, T) => Boolean), coll: Iterable[T]): 
				Boolean = coll.toList match {
			case Nil => true
			case x :: xs => xs.foldLeft(true, x)((a_cur, e) => a_cur match {
					case (a, cur) => (cmpfn(cur, e) && a, e) })._1
		}
        val (xs, arr) = (ms.toBuffer, ms.toArray)
        val (ansA, ansL) = (verifyfn[Integer]((_ <= _), arr),
            verifyfn[Integer]((_ <= _), xs))
        val (tmpA, tmpL) = (arr.map(identity), xs.map(identity))
        SeqopsArr.quickSort_lp[Integer](tmpA, 0, tmpA.size - 1)
        Seqops.quickSort_lp[Integer](tmpL, 0, tmpL.size - 1)
        val (ansSortedA, ansSortedL) = (verifyfn[Integer]((_ <= _), tmpA),
            verifyfn[Integer]((_ <= _), tmpL))
        val funcsA = Array[(Array[Integer], Boolean) => Boolean](
            SeqopsArr.isOrdered_i, SeqopsArr.isOrdered_r)
        val funcsL = List[(Iterable[Integer], Boolean) => Boolean](
            Seqops.isOrdered_i, Seqops.isOrdered_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && (ansA == f(arr, false)) &&
            ansSortedA && f(tmpA, false) }).
            label("===propIsOrderedA(%s) : %s %s===".format(
            arr.mkString("[", ", ", "]"), ansA, ansSortedA)) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && (ansL == f(xs, false)) &&
            ansSortedL && f(tmpL, false) }).
            label("===propIsOrderedL(%s) : %s %s===".format(
            xs.mkString("[", ", ", "]"), ansL, ansSortedL))
    }
    
    property("append sequence") = forAll(genListInts(Gen.choose(0, 100)),
            genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer], ys: List[Integer]) =>
        val (arr1, arr2) = (xs.toArray, ys.toArray)
        val (ansA, ansL) = (arr1 ++ arr2, xs ++ ys)
        val funcsA = Array[(Array[Integer], Array[Integer]) =>
                Array[Integer]](SeqopsArr.append_i, SeqopsArr.append_r)
        val funcsL = List[(List[Integer], List[Integer]) => List[Integer]](
            Seqops.append_i, Seqops.append_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(arr1, arr2)) }).label(
            "===propAppendA(%s, %s) : %s===".format(
            arr1.mkString("[", ", ", "]"), arr2.mkString("[", ", ", "]"),
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(xs, ys)) }).label(
            "===propAppendL(%s, %s) : %s===".format(
            xs.mkString("[", ", ", "]"), ys.mkString("[", ", ", "]"),
            ansL.mkString("[", ", ", "]")))
    }
    
    property("interleave sequences") = forAll(genListInts(Gen.choose(0, 100)),
            genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer], ys: List[Integer]) =>
        val len_short = xs.size < ys.size match {
            case true => xs.size
            case _ => ys.size }
        val (arr1, arr2) = (xs.toArray, ys.toArray)
        val ansL = (ys.take(len_short).foldRight(
            xs.drop(len_short) ++ ys.drop(len_short),
            xs.take(len_short).reverse) { (e, acc_zss) => acc_zss match {
                case (_, Nil) => acc_zss
                case (acc, z :: zs) => (z :: e :: acc, zs) }})._1
        val ansA = ansL.toArray
        val funcsA = Array[(Array[Integer], Array[Integer]) =>
            Array[Integer]](SeqopsArr.interleave_i, SeqopsArr.interleave_r)
        val funcsL = List[(List[Integer], List[Integer]) => List[Integer]](
            Seqops.interleave_i, Seqops.interleave_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(arr1, arr2)) }).label(
            "===propInterlvA(%s, %s) : %s===".format(
            arr1.mkString("[", ", ", "]"), arr2.mkString("[", ", ", "]"),
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(xs, ys)) }).label(
            "===propInterlvL(%s, %s) : %s===".format(
            xs.mkString("[", ", ", "]"), ys.mkString("[", ", ", "]"),
            ansL.mkString("[", ", ", "]")))
    }
    
    property("map proc over 2 sequences") = forAll(
            genListInts(Gen.choose(0, 100)),
            genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer], ys: List[Integer]) =>
        val proc1 = (e1: Integer, e2: Integer) => (e1 + e2) + 2 : Integer
        val (arr1, arr2) = (xs.toArray, ys.toArray)
        val ansL = xs.zip(ys).map(e1_e2 => proc1(e1_e2._1, e1_e2._2))
        val ansA = ansL.toArray
        val funcsA = Array[((Integer, Integer) => Integer, Array[Integer],
                Array[Integer]) => Array[Integer]](SeqopsArr.map2_i, 
            SeqopsArr.map2_r)
        val funcsL = List[((Integer, Integer) => Integer, List[Integer],
            List[Integer]) => List[Integer]](Seqops.map2_i, Seqops.map2_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(proc1, arr1, arr2)) }).label(
            "===propMap2A(%s, %s) : %s===".format(
            arr1.mkString("[", ", ", "]"), arr2.mkString("[", ", ", "]"),
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(proc1, xs, ys)) }).label(
            "===propMap2L(%s, %s) : %s===".format(
            xs.mkString("[", ", ", "]"), ys.mkString("[", ", ", "]"),
            ansL.mkString("[", ", ", "]")))
    }
    
    property("zip 2 sequences") = forAll(genListInts(Gen.choose(0, 100)),
            genListInts(Gen.choose(0, 100))) {
            (xs: List[Integer], ys: List[Integer]) =>
        val (arr1, arr2) = (xs.toArray, ys.toArray)
        val ansL = xs.zip(ys)
        val ansA = ansL.toArray
        val funcsA = Array[(Array[Integer], Array[Integer]) => 
            Array[(Integer, Integer)]](SeqopsArr.zip_i, SeqopsArr.zip_r)
        val funcsL = List[(List[Integer], List[Integer]) => 
            List[(Integer, Integer)]](Seqops.zip_i, Seqops.zip_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(arr1, arr2)) }).label(
            "===propZipA(%s, %s) : %s===".format(
            arr1.mkString("[", ", ", "]"), arr2.mkString("[", ", ", "]"),
            ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(xs, ys)) }).label(
            "===propZipL(%s, %s) : %s===".format(
            xs.mkString("[", ", ", "]"), ys.mkString("[", ", ", "]"),
            ansL.mkString("[", ", ", "]")))
    }
    
    property("unzip sequence") = forAll(
            for {
                numElems <- Gen.choose(1, 100)
                elems <- Gen.containerOfN[List, (Int, Int)](numElems, 
                    for { m <- Gen.choose(0, 100) ; n <- Gen.choose(0, 100) 
                        } yield (m, n))
            } yield elems) {
            (ws: List[(Int, Int)]) =>
        val arr = ws.toArray
        val (ansL, ansA) = (ws.unzip, arr.unzip)
        val funcsA = Array[Array[(Int, Int)] => (Array[Int], Array[Int])](
            SeqopsArr.unzip_i)
        val funcsL = List[List[(Int, Int)] => (List[Int], List[Int])](
            Seqops.unzip_i)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA._1 sameElements f(arr)._1) && 
            (ansA._2 sameElements f(arr)._2) }).label(
            "===propUnzipA(%s) : (%s, %s)===".format(
            arr.mkString("[", ", ", "]"), ansA._1.mkString("[", ", ", "]"),
            ansA._2.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && (ansL == f(ws)) }).label(
            "===propUnzipL(%s) : (%s, %s)===".format(
            ws.mkString("[", ", ", "]"), ansL._1.mkString("[", ", ", "]"),
            ansL._2.mkString("[", ", ", "]")))
    }
    
    property("concat(flatten) nested sequences") = forAll(
            for {
                outerNum <- Gen.choose(1, 100)
                innerNum <- Gen.choose(1, 15)
                innerElems <- Gen.containerOfN[List, Int](innerNum, 
                    Gen.choose(0, 100))
                outerElems <- Gen.containerOfN[List, List[Int]](outerNum, 
                    innerElems)
            } yield outerElems) {
            (nss: List[List[Int]]) =>
        val arrs = nss.map(lst => lst.toArray).toArray
        val (ansL, ansA) = (List.concat(nss: _*), Array.concat(arrs: _*))
        val funcsA = Array[Array[Array[Int]] => Array[Int]](
            SeqopsArr.concat_i, SeqopsArr.concat_r)
        val funcsL = List[List[List[Int]] => List[Int]](
            Seqops.concat_i, Seqops.concat_r)
        
        (funcsA.foldLeft(true) { (acc, f) => acc && 
            (ansA sameElements f(arrs)) }).label(
            "===propConcatA(%s) : %s===".format(
            arrs.mkString("[", ", ", "]"), ansA.mkString("[", ", ", "]"))) &&
        (funcsL.foldLeft(true) { (acc, f) => acc && 
            (ansL == f(nss)) }).label(
            "===propConcatL(%s) : %s===".format(
            nss.mkString("[", ", ", "]"), ansL.mkString("[", ", ", "]")))
    }
}

}

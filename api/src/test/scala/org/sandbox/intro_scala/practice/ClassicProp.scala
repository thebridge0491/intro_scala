package org.sandbox.intro_scala.practice {

import org.scalacheck.{Prop,Properties,Gen}
import org.scalacheck.Prop._

import org.sandbox.intro_scala.util.{Library => Util}
import org.sandbox.intro_scala.practice.{ClassicHiorder => ClassicHi,
	ClassicStreams => ClassicStrm}

class ClassicProp extends UnitPropSpec {
	import scala.language.implicitConversions
	import org.scalacheck.{Test => SchkTest}

	implicit def doCheck(p: org.scalacheck.Prop): Boolean = {
		SchkTest.check(SchkTest.Parameters.default, p).passed
        //SchkTest.check(SchkTest.Parameters.defaultVerbose, p).passed
	}
    
	// (from scalatest) execute scalacheck-style propert(y|ies) check(s)
    //ClassicProp.main(Array())
    ClassicProp.properties.foreach { (name_prop:(String, Prop)) => 
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
object ClassicProp extends Properties("(props) Classic functions") {
    //import scala.language.implicitConversions
    
    implicit val chooseInteger: Gen.Choose[Integer] = new Gen.Choose[Integer] {
		def choose(low: Integer, high: Integer) =
			Gen.Choose.chooseInt.choose(low.intValue, high.intValue).map(
				e => Integer.valueOf(e))
	}
    
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 0.001 //1.0e-7f
    
    def genTup2Float(gen0: Gen[Int], gen1: Gen[Int]) = 
        for { x <- gen0.map(_.toFloat) ; y <- gen1.map(_.toFloat)
            } yield (x, y)
    
    
    // scalacheck-style property define
    property("square n") = forAll(Gen.choose(1, 20)) { n =>
        val ans = math.pow(n.toFloat, 2.0f).toFloat
		val funcs = Array[Float => Float](Classic.square_i, Classic.square_r,
            ClassicHi.square_f, ClassicHi.square_u, ClassicHi.square_lc)
        val funcsStrm = Array[LazyList[Float]](ClassicStrm.squares_strm,
            ClassicStrm.squares_map2, ClassicStrm.squares_u,
            ClassicStrm.squares_scanl)
        (funcs.foldLeft(true) { (acc, f) => acc && 
			Util.in_epsilon(ans, f(n.toFloat), ans * epsilon) }).
			label("===propSquare(%f) : %f===".format(n.toFloat, ans)) &&
        (funcsStrm.foldLeft(true) { (acc, f) => acc && 
			Util.in_epsilon(ans, f(n), ans * epsilon) }).
			label("===propSquareStrm(%f) : %f===".format(n.toFloat, ans))
	}
    
    property("exponent b to n") = forAll(genTup2Float(Gen.choose(1, 20),
            Gen.choose(1, 10))) { (b_n:(Float, Float)) => 
            b_n match { case (b, n) =>
        val ans = math.pow(b, n).toFloat
		val funcs = Array[(Float, Float) => Float](Classic_java.expt_i, 
            Classic_java.expt_lp, Classic.expt_i, Classic.expt_r,
            ClassicHi.expt_f, ClassicHi.expt_u, ClassicHi.expt_lc)
        val funcsStrm = Array[Float => LazyList[Float]](ClassicStrm.expts_strm,
            ClassicStrm.expts_map2, ClassicStrm.expts_u, 
            ClassicStrm.expts_scanl)
        (funcs.foldLeft(true) { (acc, f) => acc && 
			Util.in_epsilon(ans, f(b, n), ans * epsilon) }).
			label("===propExpt(%f, %f) : %f===".format(b, n, ans)) &&
        (funcsStrm.foldLeft(true) { (acc, f) => acc && 
			Util.in_epsilon(ans, f(b)(n.toInt), ans * epsilon) }).
			label("===propExptStrm(%f, %f) : %f===".format(b, n, ans))
	}}
    
    property("sum to hi from lo") = forAll(Gen.choose(-50, 50),
            Gen.choose(-50, 50)) { (hi, lo) =>
		val ans: Long = List.range(lo + 1, hi + 1).foldLeft(lo)(_ + _)
		val funcs = Array[(Long, Long) => Long](Classic.sumTo_i,
            Classic.sumTo_r, ClassicHi.sumTo_f, ClassicHi.sumTo_u,
			ClassicHi.sumTo_lc)
        val funcsStrm = Array[Long => LazyList[Long]](ClassicStrm.sums_map2,
            ClassicStrm.sums_u, ClassicStrm.sums_scanl)
        (funcs.foldLeft(true) { (acc, f) => acc && (ans == f(hi, lo)) }).label(
			"===propSumTo(%d, %d) : %d===".format(hi, lo, ans)) &&
        (funcsStrm.foldLeft(true) { (acc, f) => acc &&
            (ans == (if (hi > lo) f(lo)(math.abs((hi - lo).toInt)) else lo)) 
            }).label("===propSumToStrm(%d, %d) : %d===".format(hi, lo, ans))
	}
    
    property("factorial n") = forAll(Gen.choose(0, 18)) { n =>
		val ans: Long = List.range(1, n + 1).foldLeft(1L)(_ * _)
		val funcs = Array[(Long) => Long](Classic_java.fact_i,
            Classic_java.fact_lp, Classic.fact_i, Classic.fact_r,
            ClassicHi.fact_f, ClassicHi.fact_u, ClassicHi.fact_lc)
        val funcsStrm = Array[LazyList[Long]](ClassicStrm.facts_map2, 
            ClassicStrm.facts_u, ClassicStrm.facts_scanl)
        (funcs.foldLeft(true) { (acc, f) => acc && (ans == f(n)) }).label(
			"===propFact(%d) : %d===".format(n, ans)) &&
        (funcsStrm.foldLeft(true) { (acc, f) => acc && (ans == f(n)) }).label(
			"===propFactStrm(%d) : %d===".format(n, ans))
	}
    
    property("nth fibonacci number") = forAll(Gen.choose(0, 20)) { n =>
		val ans = List.range(0, n + 1).foldLeft((0, 1))((s0_s1, _) => 
            (s0_s1._1 + s0_s1._2, s0_s1._1))._2
		val funcs = Array[Int => Int](Classic.fib_i, Classic.fib_r,
            ClassicHi.fib_f, ClassicHi.fib_u, ClassicHi.fib_lc)
        val funcsStrm = Array[LazyList[Int]](ClassicStrm.fibs_map2, 
            ClassicStrm.fibs_u, ClassicStrm.fibs_scanl)
        (funcs.foldLeft(true) { (acc, f) => acc && (ans == f(n)) }).label(
			"===propFib(%d) : %d===".format(n, ans)) &&
        (funcsStrm.foldLeft(true) { (acc, f) => acc && (ans == f(n)) }).label(
			"===propFibStrm(%d) : %d===".format(n, ans))
	}
    
    property("n-rows of Pascal's triangle") = forAll(Gen.choose(0, 20)) { 
            rows =>
		val verifyNumRows = ((res: List[List[Int]]) => res.size == (rows + 1))
        val verifyLenRow = ((n: Int, r: List[Int]) => r.size == (n + 1))
        val verifySumRow = ((n: Int, r: List[Int]) => 
            r.foldLeft(0)(_ + _) == (math.pow(2.0f, n.toFloat).toInt))
		val funcs = Array[Int => List[List[Int]]](Classic.pascaltri_add, 
            Classic.pascaltri_mult, ClassicHi.pascaltri_f,
			ClassicHi.pascaltri_u, ClassicHi.pascaltri_lc)
        val funcsStrm = Array[LazyList[List[Int]]](ClassicStrm.pascalrows_map2,
			ClassicStrm.pascalrows_u, ClassicStrm.pascalrows_scanl)
        (funcs.foldLeft(true) { (acc, f) => val res = f(rows) ; acc && 
            verifyNumRows(res) && (res.foldLeft(true, 0) { (acc_n, r) => 
            acc_n match { case (acc, n) => (acc && verifyLenRow(n, r) && 
            verifySumRow(n, r), n + 1) } })._1 }).label(
			"===propPascaltri(%d)===".format(rows)) &&
        (funcsStrm.foldLeft(true) { (acc, f) =>
            val res = f.take(rows + 1).toList ; acc &&
            verifyNumRows(res) && (res.foldLeft(true, 0) { (acc_n, r) => 
            acc_n match { case (acc, n) => (acc && verifyLenRow(n, r) && 
            verifySumRow(n, r), n + 1) } })._1 }).label(
			"===propPascaltriStrm(%d)===".format(rows))
	}
    
    property("quotient|remainder") = forAll(Gen.choose(-50, 50),
            Gen.choose(-50, 50).suchThat(_ != 0)) { (n, d) =>
		val (ansQ, ansR) = (n / d, n % d)
        ((ansQ == Classic.quot_m(n, d)) && (ansR == Classic.rem_m(n, d))).label(
			"===propQuotRem(%d, %d) : %d %d===".format(n, d, ansQ, ansR))
	}
    
    property("gcd|lcm") = forAll(Gen.choose(-250, 250),
            for {
                numElems <- Gen.choose(1, 20)
                elems <- Gen.containerOfN[List, Int](numElems,
                    Gen.choose(-250, 250))
            } yield elems) { (x: Int, xs: List[Int]) =>
		def euclid(m: Int, n: Int): Int = n match {
            case 0 => math.abs(m)
            case _ => euclid(n, m % n)
        }
        val ansG = xs.foldLeft(x) { (acc, e) => euclid(acc, e) }
        val ansL = math.abs(xs.foldLeft(x) { (acc, e) =>
            acc * (e.toFloat / euclid(acc, e)).toInt })
		val funcs = Array[(List[Int] => Int, List[Int] => Int)](
            (Classic.gcd_i, Classic.lcm_i), (Classic.gcd_r, Classic.lcm_r),
            (ClassicHi.gcd_f, ClassicHi.lcm_f),
			(ClassicHi.gcd_u, ClassicHi.lcm_u))
        (funcs.foldLeft(true) { (acc, fnG_fnL) => fnG_fnL match {
            case (fnG, fnL) => acc && (ansG == fnG(x :: xs)) &&
            (ansL == fnL(x :: xs)) }}).label(
			"===propGcdLcm(%s) : %d %d===".format(
            (x :: xs).mkString("[", ", ", "]"), ansG, ansL))
	}
    
    property("number to base") = forAll(Gen.choose(2, 10),
            Gen.choose(1, 200)) { (b, n) =>
		val ans = List.range(0, (math.log(n.toFloat) / math.log(b.toFloat)
                ).toInt + 1).foldLeft((List[Int](), n))((acc_num, _) => 
                acc_num match {
            case (acc, 0) => (acc, 0)
            case (acc, num) => ((num % b) :: acc, num / b) })._1
        val funcs = Array[(Int, Int) => List[Int]](Classic.baseExpand_i,
            Classic.baseExpand_r, ClassicHi.baseExpand_f,
			ClassicHi.baseExpand_u, ClassicHi.baseExpand_lc)
        (funcs.foldLeft(true) { (acc, f) => acc && (ans == f(b, n)) }).label(
			"===propBsExpand(%d, %d) : %s===".format(b, n,
            ans.mkString("[", ", ", "]")))
	}
    
    property("base b to base 10") = forAll(Gen.choose(2, 10),
            for {
                numElems <- Gen.choose(1, 20)
                elems <- Gen.containerOfN[List, Int](numElems,
                    Gen.choose(1, 200))
            } yield elems) { (b: Int, mss: List[Int]) =>
		val ans = mss.foldRight((0, 0))((e, h_t) => h_t match {
			case (h, t) => (h + 1, t + (e * math.pow(b.toFloat, h.toFloat).toInt)) })._2
        val funcs = Array[(Int, List[Int]) => Int](Classic.baseTo10_i,
            Classic.baseTo10_r, ClassicHi.baseTo10_f,
			ClassicHi.baseTo10_u, ClassicHi.baseTo10_lc)
        (funcs.foldLeft(true) { (acc, f) => acc && 
            (ans == f(b, mss)) }).label(
			"===propBsTo10(%d, %s) : %d===".format(b,
            mss.mkString("[", ", ", "]"), ans))
	}
    
    property("range of numbers from start to stop") = forAll(
            Gen.choose(-20, 20),
            Gen.choose(-20, 20)) { (start, stop) =>
		val (ansU, ansD) = (List.range(start, stop),
            List.range(start, stop, -1))
        val funcs = Array[((Int, Int, Int) => List[Int], (Int, Int) => 
            List[Int])]((Classic.rangeStep_i, Classic.range_i), 
			(Classic.rangeStep_r, Classic.range_r),
            (ClassicHi.rangeStep_f, ClassicHi.range_f),
			(ClassicHi.rangeStep_u, ClassicHi.range_u))
        (funcs.foldLeft(true) { (acc, fnStep_fnRg) => fnStep_fnRg match {
			case (fnStep, fnRg) => acc && (ansU == fnRg(start, stop)) &&
            (ansD == fnStep(-1, start, stop)) }}).label(
			"===propRange(%d, %d) : %s===".format(start, stop,
            ansU.mkString("[", ", ", "]")))
	}
    
    property("compose 2 functions") = forAll(Gen.choose(0, 20)) { n =>
        def coll_size[T](coll: Iterable[T]): Int = coll.size
		//def powCur(b: Double)(n: Double): Double = math.pow(b, n)
		//def rangeCur(start: Int)(stop: Int): Iterable[Int] = 
		//	List.range(start, stop).toIterable
        val (pow2, range0) = (math.pow(2.0, _: Double), List.range(0, _: Int))
        val ansSqr = (pow2 compose math.sqrt _)(n.toDouble)
		val ansLen = (coll_size _ compose range0)(n)
        ((ansLen == Classic.compose1(coll_size, range0, n)) &&
            Util.in_epsilon(ansSqr,
            Classic.compose1(pow2, math.sqrt, n.toDouble), ansSqr * epsilon)).label(
			"===propCompose(%d) : %d %f===".format(n, ansLen, ansSqr))
	}
}

}

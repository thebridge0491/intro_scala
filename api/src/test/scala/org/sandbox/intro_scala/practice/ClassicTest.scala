package org.sandbox.intro_scala.practice {

//import org.scalatest._

import org.sandbox.intro_scala.util.{Library => Util}
import org.sandbox.intro_scala.practice.{ClassicHiorder => ClassicHi,
	ClassicStreams => ClassicStrm}

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
	
    it should "compute square" taggedAs(Tag2) in {
		Array(2.0f, 11.0f, 20.0f).foreach { n =>
            val ans = math.pow(n, 2.0f)
            Array[Float => Float](Classic.square_i, Classic.square_r,
                ClassicHi.square_f, ClassicHi.square_u, ClassicHi.square_lc).
                    foreach(f => 
                assertResult(true) { Util.in_epsilon(ans, f(n),
					ans * epsilon) })
            Array[LazyList[Float]](ClassicStrm.squares_strm,
                ClassicStrm.squares_map2, ClassicStrm.squares_u,
                    ClassicStrm.squares_scanl).foreach(f => 
                assertResult(true) {
                    Util.in_epsilon(ans, f(n.toInt), ans * epsilon) })
        }
	}
    
    it should "compute exponent" taggedAs(Tag1) in {
		Util.cartesian_prod(Array(2.0f, 11.0f, 20.0f), 
                Array(3.0f, 6.0f, 10.0f)).foreach { b_n =>
            val ans = math.pow(b_n(0), b_n(1))
            Array[(Float, Float) => Float](Classic_java.expt_i,
                Classic_java.expt_lp, Classic.expt_i, Classic.expt_r,
                Classic.fastExpt_i, Classic.fastExpt_r, ClassicHi.expt_f,
                ClassicHi.expt_u, ClassicHi.expt_lc).foreach(f =>
                assertResult(true) {
                    Util.in_epsilon(ans, f(b_n(0), b_n(1)), ans * epsilon) })
            Array[Float => LazyList[Float]](ClassicStrm.expts_strm,
                ClassicStrm.expts_map2, ClassicStrm.expts_u, 
                    ClassicStrm.expts_scanl).foreach(f =>
                assertResult(true) {
                    Util.in_epsilon(ans, f(b_n(0))(b_n(1).toInt), ans * epsilon) })
        }
    }
	
    it should "compute sumTo" taggedAs(Tag2) in {
		Util.cartesian_prod(Array(-15L, 0L, 150L), Array(-20L, 0L, 10L)).
			foreach { hi_lo => 
                val ans = List.range(hi_lo(1) + 1, hi_lo(0) + 1).foldLeft(hi_lo(1))(_ + _)
                Array[(Long, Long) => Long](Classic.sumTo_i, Classic.sumTo_r,
                    ClassicHi.sumTo_f, ClassicHi.sumTo_u, ClassicHi.sumTo_lc
                        ).foreach(f => 
                    assertResult(ans) { f(hi_lo(0), hi_lo(1)) } )
                Array[Long => LazyList[Long]](ClassicStrm.sums_map2,
                    ClassicStrm.sums_u, ClassicStrm.sums_scanl).foreach(f => 
                    assertResult(ans) { 
                        if (hi_lo(0) > hi_lo(1)) f(hi_lo(1))(math.abs((hi_lo(0) - hi_lo(1)).toInt))
						else hi_lo(1) } )
        }
    }
	
    it should "compute factorial" taggedAs(Tag1) in {
		Array(0L, 9L, 18L).foreach { n => 
			val ans = List.range(1, n + 1).foldLeft(1L)(_ * _)
			Array[Long => Long](Classic_java.fact_i, Classic_java.fact_lp, 
                Classic.fact_i, Classic.fact_r, ClassicHi.fact_f,
                    ClassicHi.fact_u, ClassicHi.fact_lc).foreach(f => 
                assertResult(ans) { f(n) })
            Array[LazyList[Long]](ClassicStrm.facts_map2, ClassicStrm.facts_u,
                    ClassicStrm.facts_scanl).foreach(f => 
                assertResult(ans) { f(n.toInt) })
		}
    }
	
    it should "compute nth Fibonacci number" taggedAs(Tag2) in {
		Array(0, 7, 13).foreach { n => 
			val ans = List.range(0, n + 1).foldLeft((0, 1))((s0_s1, _) => 
				(s0_s1._1 + s0_s1._2, s0_s1._1))._2
			Array[Int => Int](Classic.fib_i, Classic.fib_r, ClassicHi.fib_f,
                    ClassicHi.fib_u, ClassicHi.fib_lc).foreach(f =>
                assertResult(ans) { f(n) })
            Array[LazyList[Int]](ClassicStrm.fibs_map2, ClassicStrm.fibs_u,
                    ClassicStrm.fibs_scanl).foreach(f =>
                assertResult(ans) { f(n) })
		}
    }
	
    it should "compute n-rows of Pascal's triangle" taggedAs(Tag2) in {
		val rows = 5
        val ans = List[List[Int]](List(1), List(1, 1), List(1, 2, 1), 
			List(1, 3, 3, 1), List(1, 4, 6, 4, 1), List(1, 5, 10, 10, 5, 1))
        Array[Int => List[List[Int]]](Classic.pascaltri_add, 
            Classic.pascaltri_mult, ClassicHi.pascaltri_f,
                ClassicHi.pascaltri_u, ClassicHi.pascaltri_lc).foreach(f => 
            assertResult(ans) { f(rows) })
        Array[LazyList[List[Int]]](ClassicStrm.pascalrows_map2,
			ClassicStrm.pascalrows_u, ClassicStrm.pascalrows_scanl).foreach(
                f => 
            assertResult(ans) { f.take(rows + 1).toList })
    }
	
    it should "compute quotient|remainder" in {
        Util.cartesian_prod(Array(10, -10), Array(3, -3)).foreach { n_d =>
			assertResult(n_d(0) / n_d(1)) { Classic.quot_m(n_d(0), n_d(1)) }
			assertResult(n_d(0) % n_d(1)) { Classic.rem_m(n_d(0), n_d(1)) }
        }
    }
	
    it should "compute gcd|lcm" in {
        Array[(List[Int] => Int, List[Int] => Int)](
            (Classic.gcd_i, Classic.lcm_i), (Classic.gcd_r, Classic.lcm_r),
            (ClassicHi.gcd_f, ClassicHi.lcm_f),
            (ClassicHi.gcd_u, ClassicHi.lcm_u)).
                foreach { fnGcd_fnLcm => fnGcd_fnLcm match {
			case (fnGcd, fnLcm) =>
				assertResult(8) { fnGcd(List(24, 16)) }
				assertResult(4) { fnGcd(List(24, 16, 12)) }
				assertResult(48) { fnLcm(List(24, 16)) }
				assertResult(96) { fnLcm(List(24, 16, 32)) }
		}}
    }
	
    it should "convert number to base" in {
        Array[(Int, Int) => List[Int]](Classic.baseExpand_i,
                Classic.baseExpand_r, ClassicHi.baseExpand_f,
                ClassicHi.baseExpand_u, ClassicHi.baseExpand_lc).foreach { f =>
			assertResult(List(1, 0, 1, 1)) { f(2, 11) }
			assertResult(List(1, 1, 0, 1)) { f(4, 81) }
		}
    }
	
    it should "convert from base b to base 10" in {
        Array[(Int, List[Int]) => Int](Classic.baseTo10_i,
            Classic.baseTo10_r, ClassicHi.baseTo10_f,
            ClassicHi.baseTo10_u, ClassicHi.baseTo10_lc).foreach { f =>
			assertResult(11) { f(2, List(1, 0, 1, 1)) }
			assertResult(81) { f(4, List(1, 1, 0, 1)) }
		}
    }
	
    it should "compute number range" in {
        Array[((Int, Int, Int) => List[Int], (Int, Int) => List[Int])](
			(Classic.rangeStep_i, Classic.range_i), 
			(Classic.rangeStep_r, Classic.range_r),
            (ClassicHi.rangeStep_f, ClassicHi.range_f),
            (ClassicHi.rangeStep_u, ClassicHi.range_u)).foreach {
                fnStep_fnRg => fnStep_fnRg match {
			case (fnStep, fnRg) =>
				assertResult((0 until 5)) { fnRg(0, 5) }
				assertResult((4 until (-1, -1))) { fnStep(-1, 4, -1) }
		}}
    }
	
    it should "compute result from function composition" in {
        def coll_size[T](coll: Iterable[T]): Int = coll.size
		def powCur(n: Double)(b: Double): Double = math.pow(b, n)
		def rangeCur(start: Int)(stop: Int): Iterable[Int] = 
			List.range(start, stop).toIterable
        val ans1 = (powCur(2.0) _ compose math.sqrt _)(2.0)
		assertResult(true) { Util.in_epsilon(ans1,
			Classic.compose1[Double, Double, Double](powCur(2.0),
            math.sqrt, 2.0), ans1 * epsilon) }
		assertResult((coll_size _ compose rangeCur(0) _)(5))
			{ Classic.compose1[Int, Iterable[Int], Int](coll_size,
                rangeCur(0), 5) }
    }
}

object ClassicTest {
    
}

}

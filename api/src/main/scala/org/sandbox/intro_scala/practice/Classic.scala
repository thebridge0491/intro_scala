/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.practice {

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object Classic {
	val pracLogger = LoggerFactory.getLogger("prac")
	
	def expt_i(b: Float, n: Float): Float = {
		def iter(m: Float, acc: Float): Float = (0.0f >= m) match {
			case true => acc
			case _ => iter(m - 1.0f, b * acc)
		}
		iter(n, 1.0f)
	}
	
	def expt_r(b: Float, n: Float): Float = (0.0f >= n) match {
		case true => 1.0f
		case _ => b * expt_r(b, n - 1.0f)
	}
	
	def fastExpt_i(b: Float, n: Float): Float = {
		def iter(m: Float, acc: Float): Float = m match {
			case _ if 0.0f >= m => acc
			case _ if 0 == (m % 2) => iter(m - 2, 
				acc * math.pow(b, 2.0f).toFloat)
			case _ => iter(m - 1.0f, b * acc)
		}
		iter(n, 1.0f)
	}
	
	def fastExpt_r(b: Float, n: Float): Float = n match {
		case _ if 0.0f >= n => 1.0f
		case _ if 0 == (n % 2) => math.pow(fastExpt_r(b, n / 2), 2.0f).toFloat
		case _ => b * fastExpt_r(b, n - 1.0f)
	}
	
	def square_i(n: Float): Float = expt_i(n, 2.0f)
	
	def square_r(n: Float): Float = expt_r(n, 2.0f)
	
	private def numSeqMath_i(op: ((Long, Long) => Long), init: Long, 
			hi: Long, lo: Long): Long = {
		def iter(start: Long, acc: Long): Long = (start > hi) match {
			case true => acc
			case _ => iter(start + 1L, op(acc, start))
		}
		iter(lo, init)
	}
	
	private def numSeqMath_r(op: ((Long, Long) => Long), init: Long, 
			hi: Long, lo: Long): Long = (lo > hi) match {
		case true => init
		case _ => op(numSeqMath_r(op, init, hi, lo + 1), lo)
	}
	
	def sumTo_i(hi: Long, lo: Long): Long = 
		numSeqMath_i((_ + _), lo, hi, lo + 1)
	
	def sumTo_r(hi: Long, lo: Long): Long = 
		numSeqMath_r((_ + _), lo, hi, lo + 1)
	
	def fact_i(n: Long): Long = {
		pracLogger.info("fact_i()")
		numSeqMath_i((_ * _), 1, n, 1)
	}
	
	def fact_r(n: Long): Long = numSeqMath_r((_ * _), 1, n, 1)
	
	def fib_i(n: Int): Int = {
		def iter(sum0: Int, sum1: Int, cnt: Int): Int = cnt match {
			case 0 => sum0
			case _ => iter(sum1, sum0 + sum1, cnt - 1)
		}
		iter(0, 1, n)
	}
	
	def fib_r(n: Int): Int = (2 > n) match {
		case true => n
		case _ => fib_r(n - 2) + fib_r(n - 1)
	}
	
	def pascaltri_add(rows: Int): List[List[Int]] = {
		def nextRow(xs: List[Int]): List[Int] = 
			(0 :: xs).zip(xs ++ List(0)).map(a_b => a_b._1 + a_b._2)
		def triangle(xs: List[Int], cnt: Int): List[List[Int]] = cnt match {
			case 0 => List[List[Int]]()
			case _ => xs :: triangle(nextRow(xs), cnt - 1)
		}
		triangle(List(1), rows + 1)
	}
	
	def pascaltri_mult(rows: Int): List[List[Int]] = {
		def pascalrow(r: Int): List[Int] = {
			def iter(col: Int, xs: List[Int]): List[Int] = col match {
				case _ if r == col => xs
				case _ => xs match {
					case Nil => throw new NoSuchElementException("empty list")
					case y :: _ => iter(col + 1, 
						(y.toFloat * (r - col).toFloat / col.toFloat).floor.toInt :: xs)
				}
			}
			iter(1, List(1))
		}
		List.range(1, rows + 2).map(pascalrow)
	}
	
	def quot_rem(a: Int, b: Int): (Int, Int) = {
		val q = (a.toFloat / b.toFloat).toInt
		(q, a - (q * b))
	}
	
	def quot_m(n: Int, d: Int): Int = quot_rem(n, d)._1
	
	def rem_m(n: Int, d: Int): Int = quot_rem(n, d)._2
	
	def euclid_i(m: Int, n: Int): Int = {
		def iter(a: Int, b: Int): Int = b match {
			case 0 => math.abs(a)
			case _ => iter(b, a % b)
		}
		iter(m, n)
	}
	
	def euclid_r(m: Int, n: Int): Int = n match {
		case 0 => math.abs(m)
		case _ => euclid_r(n, m % n)
	}
	
	def gcd_i(nums: List[Int]): Int = nums match {
		case Nil => 0
		case x :: xs =>
			def iter(acc: Int, rst: List[Int]): Int = rst match {
				case Nil => math.abs(acc)
				case m :: ns => iter(euclid_i(acc, m), ns)
			}
			iter(x, xs)
	}
	
	def gcd_r(nums: List[Int]): Int = nums match {
		case Nil => 0
		case m :: Nil => math.abs(m)
		case m :: n :: rst => gcd_r(euclid_r(m, n) :: rst)
	}
	
	def lcm_i(nums: List[Int]): Int = nums match {
		case Nil => 0
		case x :: xs => 
			def iter(acc: Int, rst: List[Int]): Int = rst match {
				case Nil => math.abs(acc)
				case m :: ns => iter(acc * (m.toFloat / (euclid_i(acc, m)).
					toFloat).floor.toInt, ns)
			}
			iter(x, xs)
	}
	
	def lcm_r(nums: List[Int]): Int = nums match {
		case Nil => 0
		case m :: Nil => math.abs(m)
		case m :: n :: rst => lcm_r((m * (n.toFloat / (euclid_r(m, n)).
			toFloat).floor.toInt) :: rst)
	}
	
	def baseExpand_i(b: Int, n: Int): List[Int] = {
		def iter(q: Int, xs: List[Int]): List[Int] = q match {
			case 0 => xs
			case _ => iter(q / b, (q % b) :: xs)
		}
		iter(n, Nil)
	}
	
	def baseExpand_r(b: Int, n: Int): List[Int] = n match {
		case 0 => Nil
		case _ => baseExpand_r(b, n / b) ++ List(n % b)
	}
	
	def baseTo10_i(b: Int, nums: List[Int]): Int = {
		def iter(ys: List[Int], acc: Int, cnt: Int): Int = ys match {
			case Nil => acc
			case n :: ns => iter(ns, (acc + (n * math.pow(b.toFloat, 
				cnt.toFloat).floor.toInt)), cnt + 1)
		}
		iter(nums.reverse, 0, 0)
	}
	
	def baseTo10_r(b: Int, nums: List[Int]): Int = nums match {
		case Nil => 0
		case n :: ns => baseTo10_r(b, ns) + (n * (math.pow(
			b.toFloat, ns.size.toFloat)).floor.toInt)
	}
	
	def rangeStep_i(step: Int, start: Int, stop: Int): List[Int] = {
		val cmpOp = if (step > 0) ((a: Int, b: Int) => a >= b) 
			else ((a: Int, b: Int) => a <= b)
		def iter(cur: Int, acc: List[Int]): List[Int] = stop match {
			case _ if cmpOp(cur, stop) => acc
			case _ => iter(cur + step, cur :: acc)
		}
		iter(start, Nil).reverse
	}
	
	def rangeStep_r(step: Int, start: Int, stop: Int): List[Int] = {
		val cmpOp = if (step > 0) ((a: Int, b: Int) => a >= b) 
			else ((a: Int, b: Int) => a <= b)
		stop match {
			case _ if cmpOp(start, stop) => Nil
			case _ => start :: rangeStep_r(step, start + step, stop)
		}
	}
	
	def range_i(start: Int, stop: Int): List[Int] = 
		rangeStep_i(1, start, stop)
	
	def range_r(start: Int, stop: Int): List[Int] = 
		rangeStep_r(1, start, stop)
	
	def compose1[S, T, U](f: (T => U), g: (S => T), x: S): U = f(g(x))
    
    def main(args: Array[String]): Unit = {
        printf("fact(%d): %d\n", 5, fact_i(5))
    }
}

class Classic {
}

}

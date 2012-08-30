/** DocComment:
 * <p>Brief description.</p> */
package org.sandbox.intro_scala.practice {

object ClassicHiorder {
	/*private def euclid_i(m: Int, n: Int): Int = {
		def iter(a: Int, b: Int): Int = b match {
			case 0 => math.abs(a)
			case _ => iter(b, a % b)
		}
		iter(m, n)
	}*/
	/*private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
			List[T] = {
		def iter(cur: U, acc: List[T]): List[T] = func(cur) match {
			case None => acc
			case Some((a, new_cur)) =>  iter(new_cur, a :: acc)
		}
		iter(seed, Nil)
	}*/
    private def euclid_i(m: Int, n: Int): Int = Classic.euclid_i(m, n)
    private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
        List[T] = Sequenceops.unfoldRight_i(func, seed)
	
	def expt_f(b: Float, n: Float): Float = 
		Array.range(1, n.toInt + 1).foldLeft(1.0f)((a, _) => a * b)
	
	def square_f(n: Float): Float = expt_f(n, 2.0f)
	
	private def numSeqMath_f(op: ((Long, Long) => Long), init: Long, 
			hi: Long, lo: Long): Long = 
		Array.range(lo.toInt, hi.toInt + 1).foldLeft(init)(
			(a, e) => op(a, e))
	
	def sumTo_f(hi: Long, lo: Long): Long = 
		numSeqMath_f((_ + _), lo, hi, lo + 1)
	
	def fact_f(n: Long): Long = numSeqMath_f((_ * _), 1, n, 1)
	
	def fib_f(n: Int): Int = 
		Array.range(0, n + 1).foldLeft((0, 1))((s0_s1, _) => s0_s1 match {
			case (s0, s1) => (s0 + s1, s0)})._2
	
	def pascaltri_f(rows: Int): List[List[Int]] = 
		Array.range(1, rows + 1).foldLeft(List(List(1)))((xss, _) => 
				xss match { 
			case Nil => Nil
			case x :: xs => (0 :: x).zip(x :+ 0).map((e1_e2) =>
				e1_e2 match { case (e1, e2) => e1 + e2 }) +: xss }).reverse
	
	def gcd_f(nums: List[Int]): Int = nums match {
		case Nil => 0
		case m :: ms => ms.foldLeft(math.abs(m))((a, e) => euclid_i(a, e))
	}
	
	def lcm_f(nums: List[Int]): Int = nums match {
		case Nil => 0
		case m :: ms => ms.foldLeft(math.abs(m))((a, e) => 
            math.abs(a * (e.toFloat / euclid_i(a, e)).toInt))
	}
	
	def baseExpand_f(b: Int, n: Int): List[Int] = 
		Array.range(0, (math.round(math.log(n).toFloat / math.log(b).toFloat)
				) + 1).foldLeft((List[Int](), n))((acc_n, _) => acc_n match {
			case (acc, n) => 0 == n match {
				case true => (acc, n / b)
				case _ => ((n % b) +: acc, n / b) }})._1
	
	def baseTo10_f(b: Int, nums: List[Int]): Int = 
		nums.foldRight((0, 0))((e, h_t) => h_t match {
			case (h, t) => (h + 1, t + (e * math.pow(b.toFloat, h.toFloat).toInt)) })._2
	
	def rangeStep_f(step: Int, start: Int, stop: Int): List[Int] = {
		val cmpOp = if (step > 0) ((a: Int, b: Int) => a >= b)
			else ((a: Int, b: Int) => a <= b)
		Array.range(start, stop, step).foldLeft(List[Int]())(
			(acc, e) => cmpOp(e, stop) match {
				case true => acc
				case _ => e :: acc }).reverse
	}
	
	def range_f(start: Int, stop: Int): List[Int] = 
		rangeStep_f(1, start, stop)

	
	def expt_u(b: Float, n: Float): Float = {
		val func = ((a_bs_ct:(Float, Float, Int))) => 
				a_bs_ct match { case (a, bs, ct) => 0 > ct match {
			case true => None
			case _ => Some(a, (a * bs, bs, ct - 1))
		}}
		(unfoldRight_i[Float, (Float, Float, Int)](
			func, (1.0f, b, n.toInt)).headOption).getOrElse(1.0f)
	}
	
	def square_u(n: Float): Float = expt_u(n, 2.0f)
	
	private def numSeqMath_u(op: ((Long, Long) => Long), init: Long, 
			hi: Long, lo: Long): Long = {
		val func = ((a_n0_n1:(Long, Long, Long))) => 
				a_n0_n1 match { case (a, n0, n1) => n0 > n1 match {
			case true => None
			case _ => Some(op(a, n0), (op(a, n0), n0 + 1, n1))
		}}
		(unfoldRight_i[Long, (Long, Long, Long)](
			func, (init, lo, hi)).headOption).getOrElse(init)
	}
	
	def sumTo_u(hi: Long, lo: Long): Long = 
		numSeqMath_u((_ + _), lo, hi, lo + 1)
	
	def fact_u(n: Long): Long = numSeqMath_u((_ * _), 1, n, 1)
	
	def fib_u(n: Int): Int = {
		val func = ((s0_s1_num:(Int, Int, Int))) => s0_s1_num match {
			case (s0, s1, num) => 0 >= num match {
				case true => None
				case _ => Some(s1, (s1, s0 + s1, num - 1))
		}}
		(unfoldRight_i[Int, (Int, Int, Int)](
			func, (0, 1, n)).headOption).getOrElse(n)
	}
	
	def pascaltri_u(rows: Int): List[List[Int]] = {
		def nextRow(xs: List[Int]): List[Int] = 
			(0 :: xs).zip(xs ++ List(0)).map(a_b => a_b._1 + a_b._2)
		val func = ((xss_ct:(List[List[Int]], Int))) => 
			xss_ct match { case (xss, ct) => (xss, 0 > ct) match {
				case (_, true) | (Nil, _) => None
				case (x :: xs, _) =>  
					Some(x, (nextRow(x) :: xss, ct - 1))
		}}
		unfoldRight_i[List[Int], (List[List[Int]], Int)](
			func, (List(List[Int](1)), rows)).reverse
	}
	
	/*def euclid_u(m: Int, n: Int): Int = {
		val func = ((h_t:(Int, Int))) => 
				h_t match { case (h, t) => 0 == t match {
			case true => None
			case _ => Some(t, (t, h % t))
		}}
		(unfoldRight_i[Int, (Int, Int)](
			func, (m, n)).headOption).getOrElse(math.abs(m))
	}*/
	
	def gcd_u(nums: List[Int]): Int = {
		val func = ((acc_rst:(Int, List[Int]))) => 
				acc_rst match { case (acc, rst) => rst match {
			case Nil => None
			case b :: bs => Some(euclid_i(acc, b), (euclid_i(acc, b), bs))
		}}
		nums match {
			case Nil => 0
			case m :: ms =>
				(unfoldRight_i[Int, (Int, List[Int])](
					func, (m, ms)).headOption).getOrElse(math.abs(m))
		}
	}
	
	def lcm_u(nums: List[Int]): Int = {
		val func = ((acc_rst:(Int, List[Int]))) => 
				acc_rst match { case (acc, rst) => rst match {
			case Nil => None
			case b :: bs => Some(acc * (b.toFloat / euclid_i(acc, b)).toInt, 
				(acc * (b.toFloat / euclid_i(acc, b)).toInt, bs))
		}}
		nums match { 
			case Nil => 0
			case m :: ms =>
				math.abs((unfoldRight_i[Int, (Int, List[Int])](
					func, (m, ms)).headOption).getOrElse(m))
		}
	}
	
	def baseExpand_u(b: Int, n: Int): List[Int] = {
		val func = ((bs_num:(Int, Int))) => bs_num match { 
			case (bs, num) => 0 >= num match {
				case true => None
				case _ => Some(num % bs, (bs, num / bs))
		}}
		unfoldRight_i[Int, (Int, Int)](func, (b, n))
	}
	
	def baseTo10_u(b: Int, nums: List[Int]): Int = {
		val genBase10 = (acc: Int, bs: Int, xss: List[Int]) => 
				xss match {
			case Nil => 0
			case x :: xs => acc + (x * math.pow(bs.toFloat,
                xs.size.toFloat).toInt)
		}
		val func = ((acc_bs_xss:(Int, Int, List[Int]))) => 
				acc_bs_xss match { case (acc, bs, xss) => xss match {
			case Nil => None
			case x :: xs => Some(genBase10(acc, bs, xss), 
				(genBase10(acc, bs, xss), bs, xs))
		}}
		(unfoldRight_i[Int, (Int, Int, List[Int])](
			func, (0, b, nums))).headOption.getOrElse(0)
	}
	
	def rangeStep_u(step: Int, start: Int, stop: Int): List[Int] = {
		val cmpOp = if (step > 0) ((a: Int, b: Int) => a >= b)
			else ((a: Int, b: Int) => a <= b)
		val func = (cur: Int) => cmpOp(cur, stop) match {
			case true => None
			case _ => Some(cur, cur + step)
		}
		unfoldRight_i[Int, Int](func, start).reverse
	}
	
	def range_u(start: Int, stop: Int): List[Int] = 
		rangeStep_u(1, start, stop)
	
	
	def expt_lc(b: Float, n: Float): Float = 
		/*(for { 
			x <- 0 to n.toInt 
		} yield math.pow(b, x.toFloat).toFloat).
			reverse.headOption.getOrElse(0.0f)*/
		(0 to n.toInt).map(e => math.pow(b, e.toFloat).toFloat).
			reverse.headOption.getOrElse(0.0f)
	
	def square_lc(n: Float): Float = expt_lc(n, 2.0f)
	
	private def numSeqMath_lc(op: ((Long, Long) => Long), init: Long, 
			hi: Long, lo: Long): Long = {
		lazy val _helper_lc: Stream[Long] = 
			init #:: _helper_lc.zip(lo to hi).map(acc_e =>
				op(acc_e._1, acc_e._2))
		_helper_lc.reverse.headOption.getOrElse(lo)
	}
	
	def sumTo_lc(hi: Long, lo: Long): Long = 
		numSeqMath_lc((_ + _), lo, hi, lo + 1)
	
	def fact_lc(n: Long): Long = numSeqMath_lc((_ * _), 1, n, 1)
	
	def fib_lc(n: Int): Int = {
		lazy val _helper_lc: Stream[Int] = 
			0 #:: 1 #:: _helper_lc.zip(_helper_lc.tail).map(
				s0_s1 => s0_s1._1 + s0_s1._2)
		_helper_lc(n)
	}
	
	def pascaltri_lc(rows: Int): List[List[Int]] = {
		def nextRow(xs: List[Int]): List[Int] = 
			(0 :: xs).zip(xs ++ List(0)).map(a_b => a_b._1 + a_b._2)
		lazy val _helper_lc: Stream[List[Int]] =
			List(1) #:: _helper_lc.map(row => nextRow(row))
		_helper_lc.take(rows + 1).toList
	}
	
	def baseExpand_lc(b: Int, n: Int): List[Int] = 
		/*for {
			m <- List.range(0, (math.log(n).toFloat / math.log(b).toFloat).toInt + 1).map(i => n / math.pow(b.toFloat, i.toFloat).toInt).reverse
		} yield m % b*/
		List.range(0, (math.log(n).toFloat / math.log(b).toFloat).toInt + 1).map(i => n / math.pow(b.toFloat, i.toFloat).toInt
            ).reverse.map(m => m % b)
	
	def baseTo10_lc(b: Int, nums: List[Int]): Int = 
		/*(for { 
			(i, e) <- (0 to nums.size).zip(nums.reverse)
		} yield e * math.pow(b.toFloat, i.toFloat).toInt).sum*/
		List.range(0, nums.size + 1).zip(nums.reverse).map(
			(i_e:(Int, Int)) => i_e match { case (i, e) => 
				e * math.pow(b.toFloat, i.toFloat).toInt }).sum
}

class ClassicHiorder {
}

}

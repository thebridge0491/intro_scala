/** DocComment:
 * <p>Brief description.</p> */
package org.sandbox.intro_scala.practice {

object ClassicStreams {
	private def nextRow(xs: List[Int]): List[Int] =
		(0 :: xs).zip(xs ++ List(0)).map(a_b => a_b._1 + a_b._2)
	
	def squares_strm: LazyList[Float] =
		LazyList.from(0).map(e => math.pow(e.toFloat, 2.0f).toFloat)
	
	def expts_strm(b: Float): LazyList[Float] =
		LazyList.from(0).map(e => math.pow(b, e.toFloat).toFloat)
	
	def squares_map2: LazyList[Float] = {
		lazy val _helper: LazyList[Float] =
			0.0f #:: 1.0f #:: squares_map2.zip(LazyList.from(2)).map(
				e1_e2 => e1_e2._2.toFloat * e1_e2._2.toFloat)
		_helper
	}
	
	def expts_map2(b: Float): LazyList[Float] = {
		/*lazy val _helper: LazyList[Float] =
			1.0f #:: expts_map2(b).zip(LazyList.from(1)).map(
				e1_e2 => math.pow(b, e1_e2._2.toFloat).toFloat)*/
		lazy val _helper: LazyList[Float] =
			1.0f #:: expts_map2(b).zip(LazyList.iterate(b)(identity)).map(
				e1_e2 => e1_e2._1 * e1_e2._2)
		_helper
	}
	
	def sums_map2(lo: Long): LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			lo #:: sums_map2(lo).zip(LazyList.from(1)).map(a_b =>
				a_b._1 + a_b._2.toLong + lo)
		_helper
	}
	
	def facts_map2: LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			1L #:: facts_map2.zip(LazyList.from(1)).map(a_b => a_b._1 * a_b._2)
		_helper
	}
	
	def fibs_map2: LazyList[Int] = {
		lazy val _helper: LazyList[Int] =
			0 #:: 1 #:: fibs_map2.zip(fibs_map2.tail).map(s0_s1 =>
				s0_s1._1 + s0_s1._2)
		_helper
	}
	
	def pascalrows_map2: LazyList[List[Int]] = {
		lazy val _helper: LazyList[List[Int]] =
			List(1) #:: pascalrows_map2.map(row => nextRow(row))
		_helper
	}
	
	
	def squares_scanl: LazyList[Float] = {
		lazy val _helper: LazyList[Float] =
			LazyList.from(1).scanLeft(0.0f)((a, e) => (e * e).toFloat)
		_helper
	}
	
	def expts_scanl(b: Float): LazyList[Float] = {
		lazy val _helper: LazyList[Float] =
			1.0f #:: expts_scanl(b).scanLeft(b)((a, _) => a * b)
		_helper
	}
	
	def sums_scanl(lo: Long): LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			LazyList.from(1).scanLeft(lo)((a, e) => a + e + lo)
		_helper
	}
	
	def facts_scanl: LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			LazyList.from(1).scanLeft(1L)(_ * _)
		_helper
	}
	
	def fibs_scanl: LazyList[Int] = {
		lazy val _helper: LazyList[Int] =
			0 #:: fibs_scanl.scanLeft(1)(_ + _)
		_helper
	}
	
	def pascalrows_scanl: LazyList[List[Int]] = {
		lazy val _helper: LazyList[List[Int]] =
			List(1) #:: pascalrows_scanl.scanLeft(List(1, 1))(
				(acc, row) => nextRow(acc))
		_helper
	}
	
	
	// lazy unfoldLeft variant (LazyList vice List)
	private def unfoldLeft_strm[T, U](func: (U => Option[(T, U)]), seed: U):
			LazyList[T] = func(seed) match {
		case None => LazyList.empty[T]
		case Some((a, new_seed)) => a #:: unfoldLeft_strm(func, new_seed)
	}
	
	def squares_u: LazyList[Float] = {
		lazy val _helper: LazyList[Float] =
			unfoldLeft_strm[Float, (Float, Int)]((z_ct: (Float, Int)) =>
					z_ct match {
				case (z, ct) => Some(z, ((ct * ct).toFloat, ct + 1)) }, 
					(0.0f, 1))
		_helper
	}
	
	def expts_u(b: Float): LazyList[Float] = {
		lazy val _helper: LazyList[Float] =
			unfoldLeft_strm[Float, Float]((z: Float) => Some(z, z * b), 1.0f)
		_helper
	}
	
	def sums_u(lo: Long): LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			unfoldLeft_strm[Long, (Long, Int)]((z_ct: (Long, Int)) =>
					z_ct match {
				case (z, ct) => Some(z, (z + ct.toLong + lo, ct + 1)) }, 
					(lo, 1))
		_helper
	}
	
	def facts_u: LazyList[Long] = {
		lazy val _helper: LazyList[Long] =
			unfoldLeft_strm[Long, (Long, Int)]((z_ct: (Long, Int)) =>
					z_ct match {
				case (z, ct) => Some(z, (z * ct.toLong, ct + 1)) }, (1L, 1))
		_helper
	}
	
	def fibs_u: LazyList[Int] = {
		lazy val _helper: LazyList[Int] =
			unfoldLeft_strm[Int, (Int, Int)]((s0_s1: (Int, Int)) =>
					s0_s1 match {
				case (s0, s1) => Some(s0, (s1, s0 + s1)) }, (0, 1))
		_helper
	}
	
	def pascalrows_u: LazyList[List[Int]] = {
		lazy val _helper: LazyList[List[Int]] =
			unfoldLeft_strm[List[Int], List[Int]]((row: List[Int]) =>
				Some(row, nextRow(row)), List(1))
		_helper
	}
    
    def main(args: Array[String]): Unit = {
        printf("factStrm(%d): %d\n", 5, facts_map2(5))
    }
}

class ClassicStreams {
}

}

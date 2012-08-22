/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.practice {

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object Classic {
	val pracLogger = LoggerFactory.getLogger("prac")
	
	def fact_i(n: Long): Long = {
		def iter(m: Long, acc: Long): Long = (2L > m) match {
			case true => acc
			case _ => iter(m - 1L, m * acc)
		}
		pracLogger.info("fact_i()")
		iter(n, 1L)
	}
	
	def fact_r(n: Long): Long = (2L > n) match {
		case true => 1L
		case _ => n * fact_r(n - 1L)
	}
	
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
    
    def main(args: Array[String]): Unit = {
        printf("fact(%d): %d\n", 5, fact_i(5))
    }
}

class Classic {
}

}

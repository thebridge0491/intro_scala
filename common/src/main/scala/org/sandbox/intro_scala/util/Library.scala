package org.sandbox.intro_scala.util {

import java.util.Comparator
import scala.jdk.CollectionConverters._

/** DocComment:
 * Brief description. <p> */
object Library {
	class Cmp[T <: Comparable[T]] extends java.util.Comparator[T] {
		//def apply[T]() = asInstanceOf[Comparator[T]]
		override
		def compare(a: T, b: T): Int = {
			a.compareTo(b)
		}
	}

	class Cmp_rev[T <: Comparable[T]] extends java.util.Comparator[T] {
		override
		def compare(a: T, b: T): Int = {
			-(a.compareTo(b))
		}
	}

	val floatCmp: java.util.Comparator[Float] =
			new java.util.Comparator[Float]() {
		override
		def compare(a: Float, b: Float): Int = {
			a.compareTo(b)
		}
	}
	val intCmp: java.util.Comparator[Integer] =
			new java.util.Comparator[Integer]() {
		override
		def compare(a: Integer, b: Integer): Int = {
			a.compareTo(b)
		}
	}
	//val floatCmp = Ordering.Float.asInstanceOf[java.util.Comparator[Float]]
	//val intCmp = Ordering.Int.asInstanceOf[java.util.Comparator[Integer]]

	def mkStringInit[T](coll: java.util.Collection[T], beg: String,
			sep: String, stop: String): String = {
		/*val strBldr = new StringBuilder()

		coll.asScala.foreach { e =>
			strBldr.append( (if (0 < strBldr.size) sep else "") + e) }
		strBldr.insert(0, beg)
		strBldr.append(stop)
		strBldr.toString()*/
		coll.asScala.mkString(beg, sep, stop)
	}

	def mkString[T](coll: java.util.Collection[T]): String = {
		mkStringInit(coll, "[", ", ", "]")
	}

	def mkStringInit[K, V](map1: java.util.Map[K, V], beg: String,
			sep: String, stop: String, mapsep: String): String = {
		val coll = new java.util.ArrayList[String]()

		map1.asScala.foreach { case (k, v) => 
			coll.add(k.toString + mapsep + v.toString) }
		mkStringInit(coll, beg, sep, stop)
	}

    def in_epsilon(a: Double, b: Double, tolerance: Double = 0.001): Boolean = {
        val delta = Math.abs(tolerance)
        //(a - delta) <= b && (a + delta) >= b
		!((a + delta) < b) && !((b + delta) < a)
    }

	def cartesian_prod[T: Manifest](arr1: Array[T], arr2: Array[T]):
			Array[Array[T]] = {
		//val arr_prod = for {a <- arr1 ; b <- arr2} yield Array[T](a, b)
        val arr_prod = arr1.flatMap(a => arr2.map(b => Array[T](a, b)))
        arr_prod
	}

    def main(args: Array[String]): Unit = {
        val (arr1, arr2) = (Array[Int](0, 1, 2), Array[Int](10, 20, 30))

        val res = cartesian_prod(arr1, arr2).foldLeft("") {(acc, row) =>
            acc + (if ("" == acc) "" else ", ") + row.mkString("[", ", ", "]")}
        printf("cartesian_prod(%s, %s): [%s]\n", arr1.mkString("[", ", ", "]"),
            arr2.mkString("[", ", ", "]"), res)
    }
}

class Library {
}

}

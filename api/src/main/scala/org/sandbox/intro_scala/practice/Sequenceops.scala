/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.practice {

import java.util.Comparator
import scala.collection.mutable
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._

object Sequenceops {
	val pracLogger = LoggerFactory.getLogger("prac")
	
	def indexOf_lp[T](data: T, lst: java.util.List[T], cmp: Comparator[T]):
			Int = {
		for (i <- 0 until lst.size)
			//if (lst(i).equals(data))
			if (0 == cmp.compare(lst.asScala(i), data))
				return i
		-1
	}
	
	def reverse_i[T](lst: collection.Seq[T]): collection.Seq[T] = {
		def iter(rst: collection.Seq[T], acc: collection.Seq[T]):
				collection.Seq[T] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), rst(0) +: acc)
		}
		pracLogger.info("reverse_i(Seq[T])")
		//iter(lst, lst.genericBuilder[T].result)
        iter(lst, collection.Seq[T]())
	}
	
	// mutable version
	def swapItems[T](a: Int, b: Int, lst: java.util.List[T]): Unit = {
		val lst1 = lst.asScala
		val swap = lst1(a) ; lst1.update(a, lst1(b)) ; lst1.update(b, swap)
	}
	
	// mutable version
	def reverse_i[T](lst: java.util.List[T]): Unit = {
		pracLogger.info("reverse_i(java.util.List[T])")
		
		for (i <- 0 until (lst.size >> 1))
			swapItems(i, (lst.size - 1) - i, lst)
	}
	
	
	def indexOf_lp[T](data: T, arr: Array[T], cmp: Comparator[T]): Int = {
		for (i <- 0 until arr.length)
			//if (arr(i).equals(data))
			if (0 == cmp.compare(arr(i), data))
				return i
		-1
	}
	
	def indexOf_i[T](data: T, arr: Array[T], cmp: Comparator[T]): Int = {
		def iter(idx: Int): Int = {
			if (arr.length == idx)
				-1
			else
				//arr(idx).equals(data) match {
				(0 == cmp.compare(arr(idx), data)) match {
					case true => idx
					case _ => iter(idx + 1)
				}
		}
		iter(0)
	}
	
	def reverse_i[T: Manifest](arr: Array[T]): Array[T] = {
		def iter(rst: Array[T], acc: Array[T]): Array[T] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), rst(0) +: acc)
		}
		iter(arr, Array[T]())
	}
	
	def reverse_r[T: Manifest](arr: Array[T]): Array[T] = arr.isEmpty match {
		case true => arr
		case _ => reverse_r[T](arr.drop(1)) ++ Array[T](arr(0))
	}
	
	// mutable version
	def swapItems[T](a: Int, b: Int, arr: Array[T]): Unit = {
		val swap = arr(a) ; arr(a) = arr(b) ; arr(b) = swap
	}
	
	// mutable version
	def reverse_lp[T](arr: Array[T]): Unit = {
		pracLogger.info("reverse_lp(Array[T])")
		
		for (i <- 0 until (arr.length >> 1))
			swapItems(i, (arr.length - 1) - i, arr)
	}
    
    def main(args: Array[String]): Unit = {
        val ints = Array[Int](0, 1, 2, 3)
        printf("reverse(%s): %s\n", ints.mkString("[", ", ", "]"),
            reverse_i(ints).mkString("[", ", ", "]"))
    }
}

class Sequenceops {
}

}

/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.practice {

import java.util.Comparator

object SequenceopsArray {
	def swapItems[T](a: Int, b: Int, arr: Array[T]): Unit = {
		val swap = arr(a) ; arr(a) = arr(b) ; arr(b) = swap
	}
	
	def tabulate_i[T: Manifest](func: (Int => T), cnt: Int): Array[T] = {
		def iter(idx: Int, acc: Array[T]): Array[T] =
				1 > idx match {
			case true => acc
			case _ => iter(idx - 1, (func(idx - 1) +: acc))
		}
		iter(cnt, Array[T]())
	}

	def tabulate_r[T: Manifest](func: (Int => T), cnt: Int): Array[T] = 
			1 > cnt match {
		case true => Array[T]()
		case _ => tabulate_r(func, cnt - 1) :+ func(cnt - 1)
	}
	
	def length_i[T](arr: Array[T]): Int = {
		def iter(acc: Int, rst: Array[T]): Int = rst.isEmpty match {
			case true => acc
			case _ => iter(acc + 1, rst.drop(1))
		}
		iter(0, arr)
	}
	
	def length_r[T](arr: Array[T]): Int = arr.isEmpty match {
		case true => 0
		case _ => 1 + length_r(arr.drop(1))
	}
	
	def nth_i[T](idx: Int, arr: Array[T]): Option[T] = {
		def iter(ndx: Int, rst: Array[T]): Option[T] = 
				(0 == ndx, rst.isEmpty) match {
			case (_, true) => None
			case (true, _) => Some(rst(0))
			case (false, _) => iter(ndx - 1, rst.drop(1))
		}
		iter(idx, arr)
	}
	
	def nth_r[T](idx: Int, arr: Array[T]): Option[T] = 
			(0 == idx, arr.isEmpty) match {
		case (_, true) => None
		case (true, _) => Some(arr(0))
		case (false, _) => nth_r(idx - 1, arr.drop(1))
	}
		
	//def indexOf_lp[T](data: T, arr: Array[T], cmp: Comparator[T]): Int = {
	//	for (i <- 0 until arr.length)
	//		//if (arr(i).equals(data))
	//		if (0 == cmp.compare(arr(i), data))
	//			return i
	//	-1
	//}
	
	def index_find_i[T](idx: Int, data: T, arr: Array[T], 
			cmp: Comparator[T]): (Int, Option[T]) = {
		def iter(ndx: Int, rst: Array[T]): (Int, Option[T]) = 
				rst.length < ndx + 1 match {
			case true => (-1, None)
			case _ => (0 == cmp.compare(rst(ndx), data)) match {
				case true => (ndx, Some(rst(ndx)))
				case _ => iter(ndx + 1, rst)
			}
		}
		iter(idx, arr)
	}
	
	def index_find_r[T](idx: Int, data: T, arr: Array[T], 
			cmp: Comparator[T]): (Int, Option[T]) = 
			arr.length < idx + 1 match {
		case true => (-1, None)
		case _ => if (0 == cmp.compare(arr(idx), data)) (idx, Some(arr(idx)))
            else index_find_r(idx + 1, data, arr, cmp)
	}
	
	def indexOf_i[T](data: T, arr: Array[T], cmp: Comparator[T]): Int = 
		index_find_i(0, data, arr, cmp)._1
	
	def indexOf_r[T](data: T, arr: Array[T], cmp: Comparator[T]): Int = 
		index_find_r(0, data, arr, cmp)._1
	
	def find_i[T](data: T, arr: Array[T], cmp: Comparator[T]): Option[T] = 
		index_find_i(0, data, arr, cmp)._2
	
	def find_r[T](data: T, arr: Array[T], cmp: Comparator[T]): Option[T] = 
		index_find_r(0, data, arr, cmp)._2
	
	//def min_max_i[T <% Ordered[T]](arr: Array[T]): (T, T) = 
	//		arr.isEmpty match {
	def min_max_i[T](arr: Array[T])(implicit ev: T => Ordered[T]): (T, T) = 
			arr.isEmpty match {
		case true => throw new NoSuchElementException("empty array")
		case _ =>
			def iter(lo: T, hi: T, rst: Array[T]): (T, T) = 
					rst.isEmpty match {
				case true => (lo, hi)
				case _ if lo > rst(0) => iter(rst(0), hi, rst.drop(1))
				case _ if hi < rst(0) => iter(lo, rst(0), rst.drop(1))
				case _ => iter(lo, hi, rst.drop(1))
			}
			iter(arr(0), arr(0), arr.drop(1))
	}
	
	//def min_max_r[T <% Ordered[T]: Manifest](arr: Array[T]): (T, T) = 
	//		arr.isEmpty match {
	def min_max_r[T: Manifest](arr: Array[T])(implicit ev: T => Ordered[T]): (T, T) = 
			arr.isEmpty match {
		case true => throw new NoSuchElementException("empty array")
		case _ =>
			def _helper(boolOp: ((T, T) => Boolean), rst: Array[T]):
					T = 1 == rst.length match {
				case true => rst(0)
				case _ => boolOp(rst(0), rst(1)) match {
					case true => _helper(boolOp, (rst(0) +: rst.drop(2)))
					case _ =>  _helper(boolOp, (rst(1) +: rst.drop(2)))
				}
			}
			(_helper((_ < _), arr), _helper((_ > _), arr))
	}
	
	//def min_i[T <% Ordered[T]](arr: Array[T]): T = min_max_i(arr)._1
	def min_i[T](arr: Array[T])(implicit ev: T => Ordered[T]): T = min_max_i(arr)._1
	
	//def min_r[T <% Ordered[T]: Manifest](arr: Array[T]): T = min_max_r(arr)._1
	def min_r[T: Manifest](arr: Array[T])(implicit ev: T => Ordered[T]): T = min_max_r(arr)._1
	
	//def max_i[T <% Ordered[T]](arr: Array[T]): T = min_max_i(arr)._2
	def max_i[T ](arr: Array[T])(implicit ev: T => Ordered[T]): T = min_max_i(arr)._2
	
	//def max_r[T <% Ordered[T]: Manifest](arr: Array[T]): T = min_max_r(arr)._2
	def max_r[T: Manifest](arr: Array[T])(implicit ev: T => Ordered[T]): T = min_max_r(arr)._2
	
	def reverse_i[T: Manifest](arr: Array[T]): Array[T] = {
		def iter(rst: Array[T], acc: Array[T]): Array[T] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), (rst(0) +: acc).asInstanceOf[Array[T]])
		}
		iter(arr, Array[T]())
	}
	
	def reverse_r[T: Manifest](arr: Array[T]): Array[T] = arr.isEmpty match {
		case true => arr
		case _ => (reverse_r(arr.drop(1)) :+ arr(0)).asInstanceOf[Array[T]]
	}
	
	// mutable reverse version(s)
	def reverse_mut_i[T](arr: Array[T]): Unit = {
		def iter(i: Int, j: Int): Unit = i >= j match {
			case true => return
			case _ => 
				swapItems(i, j, arr)
				iter(i + 1, j - 1)
		}
		iter(0, arr.size - 1)
	}
	
	def reverse_mut_lp[T](arr: Array[T]): Unit = {
		for (i <- 0 until (arr.length >> 1))
			swapItems(i, (arr.length - 1) - i, arr)
	}
	
	// arr.map(identity)
	def copyOf_i[T: Manifest](arr: Array[T]): Array[T] = {
		def iter(rst: Array[T], acc: Array[T]): 
				Array[T] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), (acc :+ rst(0)).asInstanceOf[Array[T]])
		}
		iter(arr, Array[T]())
	}
	
	def copyOf_r[T: Manifest](arr: Array[T]): Array[T] = arr.isEmpty match {
		case true => arr
		case _ => (arr(0) +: copyOf_r(arr.drop(1))).asInstanceOf[Array[T]]
	}
	
	def splitAt_i[T: Manifest](n: Int, arr: Array[T]): 
			(Array[T], Array[T]) = {
		def iter(m: Int, acc: Array[T], rst: Array[T]): (Array[T], Array[T]) 
				= (0 == m, rst.isEmpty) match {
			case (_, true) | (true, _) => (acc, rst)
			case (false, _) => iter(m - 1, acc :+ rst(0), rst.drop(1))
		}
		iter(n, Array[T](), arr)
	}
	
	def take_i[T: Manifest](n: Int, arr: Array[T]): Array[T] = 
		splitAt_i(n, arr)._1
	
	def drop_i[T: Manifest](n: Int, arr: Array[T]): Array[T] = 
		splitAt_i(n, arr)._2
	
	def exists_forall_i[T](pred: (T => Boolean), arr: Array[T]):
			(Boolean, Boolean) = {
		def iter(a0: Boolean, a1: Boolean, rst: Array[T]):
				(Boolean, Boolean) = rst.isEmpty match {
			case true => (a0, a1)
			case _ => iter(a0 || pred(rst(0)), a1 && pred(rst(0)), 
				rst.drop(1))
		}
		iter(false, true, arr)
	}
	
	def exists_forall_r[T](pred: (T => Boolean), arr: Array[T]):
			(Boolean, Boolean) = arr.isEmpty match {
		case true => (false, true)
		case _ => (pred(arr(0)) || (exists_forall_r(pred, arr.drop(1)))._1,
			pred(arr(0)) && (exists_forall_r(pred, arr.drop(1)))._2)
	}
	
	def exists_i[T](pred: (T => Boolean), arr: Array[T]): Boolean = 
		exists_forall_i(pred, arr)._1
	
	def exists_r[T](pred: (T => Boolean), arr: Array[T]): Boolean = 
		exists_forall_r(pred, arr)._1
	
	def forall_i[T](pred: (T => Boolean), arr: Array[T]): Boolean = 
		exists_forall_i(pred, arr)._2
	
	def forall_r[T](pred: (T => Boolean), arr: Array[T]): Boolean = 
		exists_forall_r(pred, arr)._2
	
	def map_i[T, U: Manifest](func: (T => U), arr: Array[T]): 
			Array[U] = {
		def iter(rst: Array[T], acc: Array[U]):
				Array[U] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), (acc :+ func(rst(0))))
		}
		iter(arr, Array[U]())
	}
	
	def map_r[T, U: Manifest](func: (T => U), arr: Array[T]): 
			Array[U] = arr.isEmpty match {
		case true => Array[U]()
		case _ => func(arr(0)) +: map_r(func, arr.drop(1))
	}
	
	def foreach_i[T](func: (T => Unit), arr: Array[T]): 
			Unit = {
		def iter(rst: Array[T]): Unit = rst.isEmpty match {
			case true => ()
			case _ => func(rst(0)) ; iter(rst.drop(1))
		}
		iter(arr)
	}
	
	def foreach_r[T](func: (T => Unit), arr: Array[T]): 
			Unit = arr.isEmpty match {
		case true => ()
		case _ => func(arr(0)) ; foreach_r(func, arr.drop(1))
	}
	
	def partition_i[T: Manifest](pred: (T => Boolean), arr: Array[T]):
			(Array[T], Array[T]) = {
		def iter(rst: Array[T], acc: (Array[T], Array[T])): 
				(Array[T], Array[T]) = rst.isEmpty match {
			case true => acc
			case _ => pred(rst(0)) match {
				case true => iter(rst.drop(1), (rst(0) +: acc._1, acc._2))
				case _ => iter(rst.drop(1), (acc._1, rst(0) +: acc._2))
			}
		}
		iter(arr.reverse, (Array[T](), Array[T]()))
	}
	
	def partition_r[T: Manifest](pred: (T => Boolean), arr: Array[T]):
			(Array[T], Array[T]) = {
		def _helper(boolOp: (Boolean => Boolean), rst: Array[T]):
				Array[T] = rst.isEmpty match {
			case true => rst
			case _ => boolOp(pred(rst(0))) match {
				case true => rst(0) +: _helper(boolOp, rst.drop(1))
				case _ => _helper(boolOp, rst.drop(1))
			}
		}
		(_helper(identity, arr), _helper((b => !b), arr))
	}
	
	def filter_i[T: Manifest](pred: (T => Boolean), arr: Array[T]): 
			Array[T] = partition_i(pred, arr)._1
	
	def filter_r[T: Manifest](pred: (T => Boolean), arr: Array[T]): 
			Array[T] = partition_r(pred, arr)._1
	
	def remove_i[T: Manifest](pred: (T => Boolean), arr: Array[T]): 
			Array[T] = partition_i(pred, arr)._2
	
	def remove_r[T: Manifest](pred: (T => Boolean), arr: Array[T]): 
			Array[T] = partition_r(pred, arr)._2
	
	def foldLeft_i[T, U](init: U, corp: ((U, T) => U), arr: Array[T]): U = {
		def iter(acc: U, rst: Array[T]): U = rst.isEmpty match {
			case true => acc
			case _ => iter(corp(acc, rst(0)), rst.drop(1))
		}
		iter(init, arr)
	}
	
	def foldLeft_r[T, U](init: U, corp: ((U, T) => U), arr: Array[T]): U = 
			arr.isEmpty match {
		case true => init
		case _ => foldLeft_r(corp(init, arr(0)), corp, arr.drop(1))
	}
	
	def foldRight_i[T, U](init: U, proc: ((T, U) => U), arr: Array[T]): U = {
		def iter(rst: Array[T], acc: U): U = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), proc(rst(0), acc))
		}
		iter(arr.reverse, init)
	}
	
	def foldRight_r[T, U](init: U, proc: ((T, U) => U), 
			arr: Array[T]): U = arr.isEmpty match {
		case true => init
		case _ => proc(arr(0), foldRight_r(init, proc, arr.drop(1)))
	}
	
	def unfoldRight_i[T: Manifest, U](func: (U => Option[(T, U)]), seed: U): 
			Array[T] = {
		def iter(cur: U, acc: Array[T]): Array[T] = func(cur) match {
			case None => acc
			case Some((a, new_cur)) => 
				iter(new_cur, (a +: acc))
		}
		iter(seed, Array[T]())
	}
	
	def unfoldLeft_r[T: Manifest, U](func: (U => Option[(T, U)]), seed: U): 
			Array[T] = func(seed) match {
		case None => Array[T]()
		case Some((a, new_seed)) => a +: unfoldLeft_r(func, new_seed)
	}
	
	//def isOrdered_i[T <% Ordered[T]](arr: Array[T], isRev: Boolean = false): 
	//		Boolean = {
	def isOrdered_i[T](arr: Array[T], isRev: Boolean = false
			)(implicit ev: T => Ordered[T]): Boolean = {
		def iter(acc: Boolean, rst: Array[T]): Boolean = (isRev, rst.size) match {
            case (_, 0) | (_, 1) => acc
            case (false, _) => iter(acc && rst(0) <= rst(1), rst.drop(1))
            case (true, _) => iter(acc && rst(0) >= rst(1), rst.drop(1))
        }
        iter(true, arr)
	}
	
	//def isOrdered_r[T <% Ordered[T]](arr: Array[T], isRev: Boolean = false): 
	//		Boolean = (isRev, arr.size) match {
	def isOrdered_r[T](arr: Array[T], isRev: Boolean = false
			)(implicit ev: T => Ordered[T]): Boolean = (isRev, arr.size) match {
		case (_, 0) | (_, 1) => true
		case (false, _) => arr(0) <= arr(1) && isOrdered_r(arr.drop(1), isRev)
		case (true, _) => arr(0) >= arr(1) && isOrdered_r(arr.drop(1), isRev)
	}
	
	
	//private def qpartition_lp[T <% Ordered[T]](arr: Array[T], lo: Int, 
	//		hi: Int): Int = {
	private def qpartition_lp[T](arr: Array[T], lo: Int, hi: Int
			)(implicit ev: T => Ordered[T]): Int = {
		var (lwr, upr) = (lo, hi)
		
		while (lwr < upr) {
			while (arr(lwr) <= arr(lo) && lwr < upr) {
				lwr += 1
			}
			while (arr(upr) > arr(lo)) {
				upr -= 1
			}
			if (lwr < upr) {
				swapItems(lwr, upr, arr)
			}
		}
		swapItems(lo, upr, arr)
		upr
	}
	
	//def quickSort_lp[T <% Ordered[T]](arr: Array[T], lo: Int,
	//		hi: Int): Unit = {
	def quickSort_lp[T](arr: Array[T], lo: Int, hi: Int
			)(implicit ev: T => Ordered[T]): Unit = {
		val rnd = new scala.util.Random(System.currentTimeMillis().toInt)
		if (hi > lo) {
			val rNdx = rnd.nextInt(hi - lo + 1) + lo
			swapItems(lo, rNdx, arr)
			val split = qpartition_lp(arr, lo, hi)
			quickSort_lp(arr, lo, split - 1)
			quickSort_lp(arr, split + 1, hi)
		}
	}
	
	
	def append_i[T: Manifest](arr1: Array[T], arr2: Array[T]): Array[T] = {
		def iter(rst: Array[T], acc: Array[T]): Array[T] = rst.isEmpty match {
			case true => acc
			case _ => iter(rst.drop(1), rst(0) +: acc)
		}
		iter(arr1.reverse, arr2)
	}
	
	def append_r[T: Manifest](arr1: Array[T], arr2: Array[T]): Array[T] = 
			arr1.isEmpty match {
		case true => arr2
		case _ => arr1(0) +: append_r(arr1.drop(1), arr2)
	}
	
	def interleave_i[T: Manifest](arr1: Array[T], arr2: Array[T]): 
			Array[T] = {
		def iter(rst1: Array[T], rst2: Array[T], acc: Array[T]): Array[T] =
				(rst1.isEmpty, rst2.isEmpty) match {
			case (_, true) => acc.reverse ++ rst1
			case (true, _) => acc.reverse ++ rst2
			case _ => iter(rst1.drop(1), rst2.drop(1), 
				rst2(0) +: (rst1(0) +: acc))
		}
		iter(arr1, arr2, Array[T]())
	}
	
	def interleave_r[T: Manifest](arr1: Array[T], arr2: Array[T]): 
			Array[T] = (arr1.isEmpty, arr2.isEmpty) match {
		case (_, true) => arr1
		case (true, _) => arr2
		case _	=> arr1(0) +: interleave_r(arr2, arr1.drop(1))
	}
	
	def map2_i[S, T, U: Manifest](proc: ((S, T) => U), arr1: Array[S], 
			arr2: Array[T]): Array[U] = {
		def iter(rst1: Array[S], rst2: Array[T], acc: Array[U]):
				Array[U] = (rst1.isEmpty, rst2.isEmpty) match {
			case (true, _) | (_, true) => acc
			case _ => iter(rst1.drop(1), rst2.drop(1), 
				proc(rst1(0), rst2(0)) +: acc)
		}
		iter(arr1, arr2, Array[U]()).reverse
	}

	def map2_r[S, T, U: Manifest](proc: ((S, T) => U), arr1: Array[S], 
			arr2: Array[T]): Array[U] = (arr1.isEmpty, arr2.isEmpty) match {
		case (true, _) | (_, true) => Array[U]()
		case _ => proc(arr1(0), arr2(0)) +: 
			map2_r(proc, arr1.drop(1), arr2.drop(1))
	}

	def zip_i[S: Manifest, T: Manifest](arr1: Array[S], arr2: Array[T]): 
			Array[(S, T)] = map2_i(((e1: S, e2: T) => (e1, e2)), arr1, arr2)

	def zip_r[S: Manifest, T: Manifest](arr1: Array[S], arr2: Array[T]): 
			Array[(S, T)] = map2_r(((e1: S, e2: T) => (e1, e2)), arr1, arr2)
	
	def unzip_i[T: Manifest, U: Manifest](arr: Array[(T, U)]): 
			(Array[T], Array[U]) = {
		def iter(rst: Array[(T, U)], acc: (Array[T], Array[U])): 
				(Array[T], Array[U]) = rst.isEmpty match {
			case true => acc
			case _ => 
				iter(rst.drop(1), (rst(0)._1 +: acc._1, rst(0)._2 +: acc._2))
		}
		iter(arr.reverse, (Array[T](), Array[U]()))
	}
	
	def concat_i[T: Manifest](arr: Array[Array[T]]): Array[T] = 
			arr.isEmpty match {
		case true => Array[T]()
		case _ => 
			def iter(rst: Array[Array[T]], acc: Array[T]): Array[T] =
					rst.isEmpty match {
				case true => acc.reverse
				case _ => iter(rst.drop(1), rst(0).reverse ++ acc)
			}
			iter(arr.drop(1), arr(0).reverse)
	}
	
	def concat_r[T: Manifest](arr: Array[Array[T]]): Array[T] = 
			arr.isEmpty match {
		case true => Array[T]()
		case _ => arr(0) ++ concat_r(arr.drop(1))
	}
    
    def main(args: Array[String]): Unit = {
        val ints = Array[Int](0, 1, 2, 3)
        printf("reverse(%s): %s\n", ints.mkString("[", ", ", "]"),
            reverse_i(ints).mkString("[", ", ", "]"))
    }
}

class SequenceopsArray {
}

}

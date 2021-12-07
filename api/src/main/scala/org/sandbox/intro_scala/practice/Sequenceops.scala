/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.practice {

import java.util.Comparator
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

object Sequenceops {
	val pracLogger = LoggerFactory.getLogger("prac")
	
	def swapItems[T](a: Int, b: Int, lst: Buffer[T]): Unit = {
		val swap = lst(a) ; lst.update(a, lst(b)) ; lst.update(b, swap)
	}
	
	def tabulate_i[T](func: (Int => T), cnt: Int): List[T] = {
		def iter(idx: Int, acc: List[T]): List[T] = 1 > idx match {
			case true => acc
			case _ => iter(idx - 1, func(idx - 1) :: acc)
		}
		iter(cnt, Nil)
	}
	
	def tabulate_r[T](func: (Int => T), cnt: Int): List[T] = 
			1 > cnt match {
		case true => Nil
		case _ => tabulate_r(func, cnt - 1) :+ func(cnt - 1)
	}
	
	def length_i[T](lst: List[T]): Int = {
		def iter(acc: Int, rst: List[T]): Int = rst match {
			case Nil => acc
			case _ :: ys => iter(acc + 1, ys)
		}
		iter(0, lst)
	}
	
	def length_r[T](lst: List[T]): Int = lst match {
		case Nil => 0
		case _ :: ys => 1 + length_r(ys)
	}
	
	def nth_i[T](idx: Int, lst: List[T]): Option[T] = {
		def iter(ndx: Int, rst: List[T]): Option[T] = (0 == ndx, rst) match {
			case (_, Nil) => None
			case (true, x :: _) => Some(x)
			case (false, _ :: xs) => iter(ndx - 1, xs)
		}
		iter(idx, lst)
	}
	
	def nth_r[T](idx: Int, lst: List[T]): Option[T] = (0 == idx, lst) match {
		case (_, Nil) => None
		case (true, x :: _) => Some(x)
		case (false, _ :: xs) => nth_r(idx - 1, xs)
	}
	
	//def indexOf_lp[T](data: T, lst: java.util.List[T], cmp: Comparator[T]):
	//		Int = {
	//	for (i <- 0 until lst.size)
	//		//if (lst(i).equals(data))
	//		if (0 == cmp.compare(lst.asScala(i), data))
	//			return i
	//	-1
	//}
	
	def index_find_i[T](idx: Int, data: T, lst: List[T], 
			cmp: Comparator[T]): (Int, Option[T]) = {
		def iter(ndx: Int, rst: List[T]): (Int, Option[T]) = rst match {
			case Nil => (-1, None)
			case y :: ys => (0 == cmp.compare(y, data)) match {
				case true => (ndx, Some(y))
				case _ => iter(ndx + 1, ys)
			}
		}
		iter(idx, lst)
	}
	
	def index_find_r[T](idx: Int, data: T, lst: List[T], 
			cmp: Comparator[T]): (Int, Option[T]) = lst match {
		case Nil => (-1, None)
		case y :: ys => if (0 == cmp.compare(y, data)) (idx, Some(y))
            else index_find_r(idx + 1, data, ys, cmp)
	}
	
	def indexOf_i[T](data: T, lst: List[T], cmp: Comparator[T]): Int = 
		index_find_i(0, data, lst, cmp)._1
	
	def indexOf_r[T](data: T, lst: List[T], cmp: Comparator[T]): Int = 
		index_find_r(0, data, lst, cmp)._1
	
	def find_i[T](data: T, lst: List[T], cmp: Comparator[T]): Option[T] = 
		index_find_i(0, data, lst, cmp)._2
	
	def find_r[T](data: T, lst: List[T], cmp: Comparator[T]): Option[T] = 
		index_find_r(0, data, lst, cmp)._2
	
	//def min_max_i[T <% Ordered[T]](lst: List[T]): (T, T) = lst match {
	def min_max_i[T](lst: List[T])(implicit ev: T => Ordered[T]): (T, T) = lst match {
		case Nil => throw new NoSuchElementException("empty list")
		case x :: xs =>
			def iter(lo: T, hi: T, rst: List[T]): (T, T) = rst match {
				case Nil => (lo, hi)
				case y :: ys => y match {
					case _ if lo > y => iter(y, hi, ys)
					case _ if hi < y => iter(lo, y, ys)
					case _ => iter(lo, hi, ys)
				}
			}
			iter(x, x, xs)
	}
	
	//def min_max_r[T <% Ordered[T]](lst: List[T]): (T, T) = {
	def min_max_r[T](lst: List[T])(implicit ev: T => Ordered[T]): (T, T) = {
		def _helper(boolOp: ((T, T) => Boolean), rst: List[T]): T = 
				rst match {
			case Nil => throw new NoSuchElementException("empty list")
			case x :: Nil => x
			case x :: y :: ys => boolOp(x, y) match {
				case true => _helper(boolOp, x :: ys)
				case _ =>  _helper(boolOp, y :: ys)
			}
		}
		(_helper((_ < _), lst), _helper((_ > _), lst))
	}
	
	//def min_i[T <% Ordered[T]](lst: List[T]): T = min_max_i(lst)._1
	def min_i[T](lst: List[T])(implicit ev: T => Ordered[T]): T = 
		min_max_i(lst)._1
	
	//def min_r[T <% Ordered[T]](lst: List[T]): T = min_max_r(lst)._1
	def min_r[T](lst: List[T])(implicit ev: T => Ordered[T]): T = 
		min_max_r(lst)._1
	
	//def max_i[T <% Ordered[T]](lst: List[T]): T = min_max_i(lst)._2
	def max_i[T](lst: List[T])(implicit ev: T => Ordered[T]): T = 
		min_max_i(lst)._2
	
	//def max_r[T <% Ordered[T]](lst: List[T]): T = min_max_r(lst)._2
	def max_r[T](lst: List[T])(implicit ev: T => Ordered[T]): T = 
		min_max_r(lst)._2
	
	def reverse_i[T](lst: List[T]): List[T] = {
		def iter(rst: List[T], acc: List[T]): List[T] = 
				rst match {
			case Nil => acc
			case x :: xs => iter(xs, x +: acc)
		}
		pracLogger.info("reverse_i()")
		iter(lst, Nil)
	}
	
	def reverse_r[T](lst: List[T]): List[T] = lst match {
		case Nil => Nil
		case x :: xs => reverse_r(xs) :+ x
	}
	
	// mutable reverse version(s)
	def reverse_mut_i[T](lst: Buffer[T]): Unit = {
		def iter(i: Int, j: Int): Unit = i >= j match {
			case true => return
			case _ => 
				swapItems(i, j, lst)
				iter(i + 1, j - 1)
		}
		iter(0, lst.size - 1)
	}
	
	def reverse_mut_lp[T](lst: Buffer[T]): Unit = {
		pracLogger.info("reverse_mut_lp()")
		
		for (i <- 0 until (lst.size >> 1))
			swapItems(i, (lst.size - 1) - i, lst)
	}
	
	// lst.map(identity)
	def copyOf_i[T](lst: List[T]): List[T] = {
		def iter(rst: List[T], acc: List[T]): List[T] = 
				rst match {
			case Nil => acc
			case x :: xs => iter(xs, acc :+ x)
		}
		iter(lst, Nil)
	}
	
	def copyOf_r[T](lst: List[T]): List[T] = lst match {
		case Nil => Nil
		case x :: xs => x +: copyOf_r(xs)
	}
	
	def splitAt_i[T](n: Int, lst: List[T]): (List[T], List[T]) = {
		def iter(m: Int, acc: List[T], rst: List[T]): (List[T], List[T]) = 
				(0 == m, rst) match {
			case (_, Nil) | (true, _) => (acc.reverse, rst)
			case (false, y :: ys) => iter(m - 1, y :: acc, ys)
		}
		iter(n, Nil, lst)
	}
	
	def take_i[T](n: Int, lst: List[T]): List[T] = 
		splitAt_i(n, lst)._1
	
	def drop_i[T](n: Int, lst: List[T]): List[T] = 
		splitAt_i(n, lst)._2
	
	def exists_forall_i[T](pred: (T => Boolean), lst: List[T]):
			(Boolean, Boolean) = {
		def iter(a0: Boolean, a1: Boolean, rst: List[T]): (Boolean, Boolean) =
				rst match {
			case Nil => (a0, a1)
			case y :: ys => iter(a0 || pred(y), a1 && pred(y), ys)
		}
		iter(false, true, lst)
	}
	
	def exists_forall_r[T](pred: (T => Boolean), lst: List[T]):
			(Boolean, Boolean) = lst match {
		case Nil => (false, true)
		case x :: xs => (pred(x) || (exists_forall_r(pred, xs))._1,
			pred(x) && (exists_forall_r(pred, xs))._2)
	}
	
	def exists_i[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_i(pred, lst)._1
	
	def exists_r[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_r(pred, lst)._1
	
	def forall_i[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_i(pred, lst)._2
	
	def forall_r[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_r(pred, lst)._2
	
	def map_i[T, U](func: (T => U), lst: List[T]): List[U] = {
		def iter(rst: List[T], acc: List[U]): List[U] = rst match {
			case Nil => acc
			case y :: ys => iter(ys, func(y) :: acc)
		}
		iter(lst.reverse, Nil)
	}
	
	def map_r[T, U](func: (T => U), lst: List[T]): List[U] = lst match {
		case Nil => Nil
		case x :: xs => func(x) :: map_r(func, xs)
	}
	
	def foreach_i[T](func: (T => Unit), lst: List[T]): Unit = {
		def iter(rst: List[T]): Unit = rst match {
			case Nil => ()
			case y :: ys => func(y) ; iter(ys)
		}
		iter(lst)
	}
	
	def foreach_r[T](func: (T => Unit), lst: List[T]): Unit = lst match {
		case Nil => ()
		case x :: xs => func(x) ; foreach_r(func, xs)
	}
	
	def partition_i[T](pred: (T => Boolean), lst: List[T]): 
			(List[T], List[T]) = {
		def iter(rst: List[T], acc: (List[T], List[T])): (List[T], List[T]) = 
				rst match {
			case Nil => acc
			case y :: ys => pred(y) match {
				case true => iter(ys, (y :: acc._1, acc._2))
				case _ => iter(ys, (acc._1, y :: acc._2))
			}
		}
		iter(lst.reverse, (Nil, Nil))
	}
	
	def partition_r[T](pred: (T => Boolean), lst: List[T]): 
			(List[T], List[T]) = {
		def _helper(boolOp: (Boolean => Boolean), rst: List[T]): List[T] = 
				rst match {
			case Nil => rst
			case y :: ys => boolOp(pred(y)) match {
				case true => y +: _helper(boolOp, ys)
				case _ => _helper(boolOp, ys)
			}
		}
		(_helper(identity, lst), _helper((b => !b), lst))
	}
	
	def filter_i[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_i(pred, lst)._1
	
	def filter_r[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_r(pred, lst)._1
	
	def remove_i[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_i(pred, lst)._2
	
	def remove_r[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_r(pred, lst)._2
	
	def foldLeft_i[T, U](init: U, corp: ((U, T) => U), lst: List[T]): U = {
		def iter(acc: U, rst: List[T]): U = rst match {
			case Nil => acc
			case y :: ys => iter(corp(acc, y), ys)
		}
		iter(init, lst)
	}
	
	def foldLeft_r[T, U](init: U, corp: ((U, T) => U), lst: List[T]): U = 
			lst match {
		case Nil => init
		case x :: xs => foldLeft_r(corp(init, x), corp, xs)
	}
	
	def foldRight_i[T, U](init: U, proc: ((T, U) => U), lst: List[T]): U = {
		def iter(rst: List[T], acc: U): U = rst match {
			case Nil => acc
			case y :: ys => iter(ys, proc(y, acc))
		}
		iter(lst.reverse, init)
	}
	
	def foldRight_r[T, U](init: U, proc: ((T, U) => U), lst: List[T]): U = 
			lst match {
		case Nil => init
		case x :: xs => proc(x, foldRight_r(init, proc, xs))
	}
	
	def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
			List[T] = {
		def iter(cur: U, acc: List[T]): List[T] = func(cur) match {
			case None => acc
			case Some((a, new_cur)) =>  iter(new_cur, a :: acc)
		}
		iter(seed, Nil)
	}
	
	def unfoldLeft_r[T, U](func: (U => Option[(T, U)]), seed: U): 
			List[T] = func(seed) match {
		case None => Nil
		case Some((a, new_seed)) => a :: unfoldLeft_r(func, new_seed)
	}
    
    //def isOrdered_i[T <% Ordered[T]](coll: Iterable[T],
    //        isRev: Boolean = false): Boolean = {
    def isOrdered_i[T](coll: Iterable[T], isRev: Boolean = false
            )(implicit ev: T => Ordered[T]): Boolean = {
        def iter(acc: Boolean, rst: Iterable[T]): Boolean = (isRev, rst) match {
            case (_, Nil) | (_, _ :: Nil) => acc
            case (false, x :: y :: ys) => iter(acc && x <= y, y :: ys)
            case (true, x :: y :: ys) => iter(acc && x >= y, y :: ys)
            case (false, _) | (true, _) => acc
        }
        iter(true, coll.toList)
    }
	
	//def isOrdered_r[T <% Ordered[T]](coll: Iterable[T],
    //        isRev: Boolean = false): Boolean = (isRev, coll.toList) match {
	def isOrdered_r[T](coll: Iterable[T], isRev: Boolean = false
            )(implicit ev: T => Ordered[T]): Boolean = (isRev, coll.toList) match {
		case (_, Nil) | (_, _ :: Nil) => true
		case (false, x :: y :: ys) => x <= y && isOrdered_r(y :: ys, isRev)
		case (true, x :: y :: ys) => x >= y && isOrdered_r(y :: ys, isRev)
	}
	
	
	//private def qpartition_lp[T <% Ordered[T]](lst: Buffer[T], lo: Int, 
	//		hi: Int): Int = {
	private def qpartition_lp[T](lst: Buffer[T], lo: Int, hi: Int
			)(implicit ev: T => Ordered[T]): Int = {
		var (lwr, upr) = (lo, hi)
		
		while (lwr < upr) {
			while (lst(lwr) <= lst(lo) && lwr < upr) {
				lwr += 1
			}
			while (lst(upr) > lst(lo)) {
				upr -= 1
			}
			if (lwr < upr) {
				swapItems(lwr, upr, lst)
			}
		}
		swapItems(lo, upr, lst)
		upr
	}
	
	//def quickSort_lp[T <% Ordered[T]](lst: Buffer[T], lo: Int, hi: Int): 
	//		Unit = {
	def quickSort_lp[T](lst: Buffer[T], lo: Int, hi: Int)(implicit ev: T => Ordered[T]): 
			Unit = {
		val rnd = new scala.util.Random(System.currentTimeMillis().toInt)
		if (hi > lo) {
			val rNdx = rnd.nextInt(hi - lo + 1) + lo
			swapItems(lo, rNdx, lst)
			val split = qpartition_lp(lst, lo, hi)
			quickSort_lp(lst, lo, split - 1)
			quickSort_lp(lst, split + 1, hi)
		}
	}

	
	def append_i[T](lst1: List[T], lst2: List[T]): List[T] = {
		def iter(rst: List[T], acc: List[T]): List[T] = rst match {
			case Nil => acc
			case x :: xs => iter(xs, x :: acc)
		}
		iter(lst1.reverse, lst2)
	}
	
	def append_r[T](lst1: List[T], lst2: List[T]): List[T] = 
			lst1 match {
		case Nil => lst2
		case x :: xs => x :: append_r(xs, lst2)
	}
	
	def interleave_i[T](lst1: List[T], lst2: List[T]): List[T] = {
		def iter(rst1: List[T], rst2: List[T], acc: List[T]): List[T] =
				(rst1, rst2) match {
			case (xs, Nil) => acc.reverse ++ xs
			case (Nil, ys) => acc.reverse ++ ys
			case (x :: xs, y :: ys) => iter(xs, ys, y :: x :: acc)
		}
		iter(lst1, lst2, Nil)
	}
	
	def interleave_r[T](lst1: List[T], lst2: List[T]): List[T] = 
			(lst1, lst2) match {
		case (_, Nil) => lst1
		case (Nil, _) => lst2
		case (x :: xs, _) => x :: interleave_r(lst2, xs)
	}
	
	def map2_i[S, T, U](proc: ((S, T) => U), lst1: List[S], 
			lst2: List[T]): List[U] = {
		def iter(rst1: List[S], rst2: List[T], acc: List[U]): List[U] = 
				(rst1, rst2) match {
			case (Nil, _) | (_, Nil) => acc
			case (x :: xs, y :: ys) => iter(xs, ys, proc(x, y) +: acc)
		}
		iter(lst1, lst2, Nil).reverse
	}

	def map2_r[S, T, U](proc: ((S, T) => U), lst1: List[S], 
			lst2: List[T]): List[U] = (lst1, lst2) match {
		case (Nil, _) | (_, Nil) => Nil
		case (x :: xs, y :: ys) => proc(x, y) +: map2_r(proc, xs, ys)
	}

	def zip_i[S, T](lst1: List[S], lst2: List[T]): 
			List[(S, T)] = 
		map2_i(((e1: S, e2: T) => (e1, e2)), lst1, lst2)

	def zip_r[S, T](lst1: List[S], lst2: List[T]): 
			List[(S, T)] = 
		map2_r(((e1: S, e2: T) => (e1, e2)), lst1, lst2)
	
	def unzip_i[T, U](lst: List[(T, U)]): 
			(List[T], List[U]) = {
		def iter(rst: List[(T, U)], acc: (List[T], List[U])): 
				(List[T], List[U]) = rst match {
			case Nil => acc
			case y :: ys => iter(ys, 
				(y._1 +: acc._1, y._2 +: acc._2))
		}
		iter(lst.reverse, (Nil, Nil))
	}
	
	def concat_i[T](nlst: List[List[T]]): List[T] = nlst match {
		case Nil => Nil
		case x :: xs => 
			def iter(rst: List[List[T]], acc: List[T]): List[T] = rst match {
				case Nil => acc.reverse
				case y :: ys => iter(ys, y.reverse ++ acc)
			}
			iter(xs, x.reverse)
	}
	
	def concat_r[T](nlst: List[List[T]]): List[T] = nlst match {
		case Nil => Nil
		case x :: xs => x ++ concat_r(xs)
	}
    
    def main(args: Array[String]): Unit = {
        val xs = List[Int](0, 1, 2, 3)
        printf("reverse(%s): %s\n", xs.mkString("[", ", ", "]"),
            reverse_i(xs).mkString("[", ", ", "]"))
    }
}

class Sequenceops {
}

}

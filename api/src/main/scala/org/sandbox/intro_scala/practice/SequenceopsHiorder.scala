/** DocComment:
 * <p>Brief description.</p> */
package org.sandbox.intro_scala.practice {

import java.util.Comparator
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

object SequenceopsHiorder {
	/*private def swapItems[T](a: Int, b: Int, lst: Buffer[T]): Unit = {
		val swap = lst(a) ; lst.update(a, lst(b)) ; lst.update(b, swap)
	}*/
	/*private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
			List[T] = {
		def iter(cur: U, acc: List[T]): List[T] = func(cur) match {
			case None => acc
			case Some((a, new_cur)) =>  iter(new_cur, a :: acc)
		}
		iter(seed, Nil)
	}*/
    private def swapItems[T](a: Int, b: Int, lst: Buffer[T]): Unit = 
        Sequenceops.swapItems(a, b, lst)
    private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
        List[T] = Sequenceops.unfoldRight_i(func, seed)
    
	def tabulate_f[T](func: (Int => T), cnt: Int): List[T] = 
		(0 until cnt).foldLeft(List[T]())((a, i) => func(i) :: a).reverse
	
	def length_f(lst: List[_ <: Any]): Int = lst.foldLeft(0)((a, _) => a + 1)
	
	def nth_f[T](idx: Int, lst: List[T]): Option[T] = {
		def corp(h_t: (Int, Option[T]), el: T): (Int, Option[T]) = h_t match {
			case (h, t) => idx == h match {
				case true => (h + 1, Some(el))
				case _ => (h + 1, t)
			}
		}
		lst.foldLeft((0, Option.empty[T]))(corp)._2
	}
	
	def index_find_f[T](idx: Int, data: T, lst: List[T], 
			cmp: Comparator[T]): (Int, Option[T]) = {
		def corp(ndx1_ndx_it: (Int, (Int, Option[T])), el: T): 
				(Int, (Int, Option[T])) = ndx1_ndx_it match {
			case (ndx1, ndx_it) => ndx_it match { case (ndx, it) =>
				None == it && (0 == cmp.compare(el, data)) match {
					case true => (ndx1 + 1, (ndx1, Some(el)))
					case _ => (ndx1 + 1, (ndx, it))
				}
			}
		}
		lst.foldLeft((idx, (-1, Option.empty[T])))(corp)._2
	}
	
	def indexOf_f[T](data: T, lst: List[T], cmp: Comparator[T]): Int = 
		index_find_f(0, data, lst, cmp)._1
	
	def find_f[T](data: T, lst: List[T], cmp: Comparator[T]): Option[T] = 
		index_find_f(0, data, lst, cmp)._2
	
	def min_max_f[T <% Ordered[T]](lst: List[T]): (T, T) = {
		def corp(lo_hi: (T, T), e: T): (T, T) = 
				(e < lo_hi._1, e > lo_hi._2) match {
			case (true, _) => (e, lo_hi._2)
			case (_, true) => (lo_hi._1, e)
			case _ => lo_hi
		}
		lst match {
			case Nil => throw new NoSuchElementException("empty list")
			case x :: xs => xs.foldLeft((x, x))(corp)
		}
	}
	
	def min_f[T <% Ordered[T]](lst: List[T]): T = min_max_f(lst)._1
	
	def max_f[T <% Ordered[T]](lst: List[T]): T = min_max_f(lst)._2
	
	def reverse_f[T](lst: List[T]): List[T] = 
		lst.foldLeft(List[T]())((a, e) => e :: a)
	
	// mutable reverse version(s)
	def reverse_mut_f[T](lst: Buffer[T]): Unit = 
		(0 until (lst.size >> 1)).foldLeft(())(
			(_, i) => swapItems(i, (lst.size - 1) - i, lst))
	
	def copyOf_f[T](lst: List[T]): List[T] = 
		lst.foldRight(List[T]())((e, a) => e :: a)
	
	def splitAt_f[T](n: Int, lst: List[T]): (List[T], List[T]) = {
		def corp(t_ys: (List[T], List[T]), i: Int): (List[T], List[T]) =
			t_ys match { case (t, ys) => ys match {
				case Nil => (t, ys)
				case z :: zs => (z :: t, zs)
			}
		}
		val (lstT, lstD) = (0 until n).foldLeft((List[T](), lst))(corp)
		(lstT.reverse, lstD)
	}
	
	def take_f[T](n: Int, lst: List[T]): List[T] = 
		splitAt_f(n, lst)._1
	
	def drop_f[T](n: Int, lst: List[T]): List[T] = 
		splitAt_f(n, lst)._2
	
	def exists_forall_f[T](pred: (T => Boolean), lst: List[T]):
			(Boolean, Boolean) = {
		def corp(a0_a1: (Boolean, Boolean), e: T): (Boolean, Boolean) =
			(a0_a1._1 || pred(e), a0_a1._2 && pred(e))
		lst.foldLeft((false, true))(corp)
	}
	
	def exists_f[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_f(pred, lst)._1
	
	def forall_f[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_f(pred, lst)._2
	
	def map_f[T, U](proc: (T => U), lst: List[T]): List[U] = 
		lst.foldRight(List[U]())((e, acc) => proc(e) :: acc)
	
	def foreach_f[T](proc: (T => Unit), lst: List[T]): Unit = 
		lst.foldLeft(())((_, e) => proc(e))
	
	def partition_f[T](pred: (T => Boolean), lst: List[T]): 
			(List[T], List[T]) = {
		def corp(f_r: (List[T], List[T]), e: T): (List[T], List[T]) = 
			f_r match { case (f, r) => pred(e) match {
				case true => (e :: f, r)
				case _ => (f, e :: r)
			}
		}
		lst.reverse.foldLeft((List[T](), List[T]()))(corp)
	}
	
	def filter_f[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_f(pred, lst)._1
	
	def remove_f[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_f(pred, lst)._2
    
	def isOrdered_f[T <% Ordered[T]](coll: Iterable[T],
            isRev: Boolean = false): Boolean = (isRev, coll.toList) match {
		case (_, Nil) => true
		case (false, x :: xs) => 
			xs.foldLeft((true, x))((acc_cur, e) => acc_cur match {
				case (acc, cur) => (acc && cur <= e, e) })._1
		case (true, x :: xs) => 
			xs.foldLeft((true, x))((acc_cur, e) => acc_cur match {
				case (acc, cur) => (acc && cur >= e, e) })._1
	}
	
	def append_f[T](lst1: List[T], lst2: List[T]): List[T] = 
		lst1.foldRight(lst2)((e, acc) => e :: acc)
	
	def interleave_f[T](lst1: List[T], lst2: List[T]): List[T] = {
		val len_short = if (lst1.size < lst2.size) lst1.size else lst2.size
		def proc(e : T, acc_zss: (List[T], List[T])): (List[T], List[T]) = 
			acc_zss match { case (acc, zss) => zss match {
				case Nil => (acc, zss)
				case z :: zs => (z :: e :: acc, zs)
			}
		}
		lst2.take(len_short).foldRight(
			(lst1.drop(len_short) ++ lst2.drop(len_short), 
			lst1.take(len_short).reverse))(proc)._1
	}
	
	def map2_f[S, T, U](proc: ((S, T) => U), lst1: List[S], 
			lst2: List[T]): List[U] = {
		val len_short = if (lst1.size < lst2.size) lst1.size else lst2.size
		def corp(acc_xss_yss: (List[U], List[S], List[T]), i: Int): 
				(List[U], List[S], List[T]) = acc_xss_yss match {
			case (acc, xss, yss) => (xss, yss) match {
				case (x :: xs, y :: ys) => (proc(x, y) :: acc, xs, ys)
				case _ => (acc, xss, yss)
			}
		}
		(0 until len_short).foldLeft((List[U](), lst1, lst2))(corp)._1.reverse
	}

	def zip_f[S, T](lst1: List[S], lst2: List[T]): 
			List[(S, T)] = 
		map2_f(((e1: S, e2: T) => (e1, e2)), lst1, lst2)
	
	def unzip_f[T, U](lst: List[(T, U)]): 
			(List[T], List[U]) = {
		def proc(eh_et: (T, U), ah_at: (List[T], List[U])): 
				(List[T], List[U]) = (eh_et, ah_at) match {
			case ((eh, et), (ah, at)) => (eh :: ah, et :: at)
		}
		lst.foldRight((List[T](), List[U]()))(proc)
	}
	
	def concat_f[T](nlst: List[List[T]]): List[T] = 
		nlst.foldRight(List[T]())((e, a) => e ++ a)
	
	
	
	def tabulate_u[T](func: (Int => T), cnt: Int): List[T] = {
		val proc = ((acc_idx:(List[T], Int))) => acc_idx match { 
			case (acc, idx) => cnt <= idx match {
				case true => None
				case _ => Some(func(idx) :: acc, 
					(func(idx) :: acc, idx + 1))
			}
		}
		(unfoldRight_i[List[T], (List[T], Int)](
            proc, (Nil, 0)).headOption).getOrElse(Nil).reverse
	}
	
	def length_u(lst: List[_ <: Any]): Int = {
		val func = ((cnt_rst:(Int, List[_ <: Any]))) => cnt_rst match { 
			case (cnt, rst) => rst match {
				case Nil => None
				case y :: ys => Some(cnt + 1, (cnt + 1, ys))
			}
		}
		(unfoldRight_i[Int, (Int, List[_ <: Any])](
            func, (0, lst)).headOption).getOrElse(0)
	}
	
	def nth_u[T](idx: Int, lst: List[T]): Option[T] = {
		val genFunc = (ndx: Int, el: Option[T], z: T) => idx == ndx match {
			case true => Some(z)
			case _ => el
		}
		val func = ((i_e_rst:(Int, Option[T], List[T]))) => i_e_rst match { 
			case (idx, el, rst) => rst match {
				case Nil => None
				case y :: ys => Some(genFunc(idx, el, y), 
					(idx + 1, genFunc(idx, el, y), ys))
			}
		}
		(unfoldRight_i[Option[T], (Int, Option[T], List[T])](
            func, (0, None, lst)).headOption).getOrElse(None)
	}
	
	def index_find_u[T](idx: Int, data: T, lst: List[T], 
			cmp: Comparator[T]): (Int, Option[T]) = {
		val genFunc = (ndx: Int, acc: (Int, Option[T]), y: T) => 
				(0 == cmp.compare(y, data)) && acc._2 == None match {
			case true => (ndx, Some(y))
			case _ => acc
		}
		val func = ((i_acc_rst:(Int, (Int, Option[T]), List[T]))) => 
				i_acc_rst match { 
			case (idx, acc, rst) => rst match {
				case Nil => None
				case y :: ys => Some(genFunc(idx, acc, y), 
					(idx + 1, genFunc(idx, acc, y), ys))
			}
		}
		(unfoldRight_i[(Int, Option[T]), (Int, (Int, Option[T]), List[T])](
            func, (0, (-1, None), lst)).headOption).getOrElse((-1, None))
	}
	
	def indexOf_u[T](data: T, lst: List[T], cmp: Comparator[T]): 
		Int = index_find_u(0, data, lst, cmp)._1
	
	def find_u[T](data: T, lst: List[T], cmp: Comparator[T]): 
		Option[T] = index_find_u(0, data, lst, cmp)._2
	
	def min_max_u[T <% Ordered[T]](lst: List[T]): (T, T) = 
			lst match {
		case Nil => throw new NoSuchElementException("empty list")
		case x :: xs => 
			val genFunc = (lo_hi: (T, T), y: T) => lo_hi match {
				case (lo, hi) => (lo > y, hi < y) match {
					case (true, _) => (y, hi)
					case (_, true) => (lo, y)
					case _ => (lo, hi)
				}
			}
			val func = ((lo_hi_rst:((T, T), List[T]))) => lo_hi_rst match { 
				case (lo_hi, rst) => rst match {
					case Nil => None
					case y :: ys => Some(genFunc(lo_hi, y), 
						(genFunc(lo_hi, y), ys))
				}
			}
			(unfoldRight_i[(T, T), ((T, T), List[T])](
				func, ((x, x), lst)).headOption).getOrElse((x, x))
	}
	
	def min_u[T <% Ordered[T]](lst: List[T]): T = min_max_u(lst)._1
	
	def max_u[T <% Ordered[T]](lst: List[T]): T = min_max_u(lst)._2
	
	def reverse_u[T](lst: List[T]): List[T] = {
		val func = (rst: List[T]) => rst match { 
			case Nil => None
			case y :: ys => Some(y, ys)
		}
		unfoldRight_i[T, List[T]](func, lst)
	}
	
	// mutable reverse version(s)
	def reverse_mut_u[T](lst: Buffer[T]): Unit = {
		val func = (i_rst: (Int, Buffer[T])) => i_rst match { 
			case (i, rst) => i >= (rst.size >> 1) match {
				case true => None
				case _ => swapItems(i, (rst.size - 1) - i, rst)
					Some((), (i + 1, rst))
			}
		}
		unfoldRight_i[Unit, (Int, Buffer[T])](func, (0, lst))
	}
	
	def copyOf_u[T](lst: List[T]): List[T] = {
		val func = (rst: List[T]) => rst match { 
			case Nil => None
			case y :: ys => Some(y, ys)
		}
		unfoldRight_i[T, List[T]](func, lst).reverse
	}
	
	def splitAt_u[T](n: Int, lst: List[T]): (List[T], List[T]) = {
		val func = (cnt_t_d: (Int, (List[T], List[T]))) => cnt_t_d match { 
			case (cnt, t_d) => (0 == cnt, t_d) match {
				case (true, _) | (_, (_, Nil)) => None
				case (_, (t, y :: ys)) => 
					Some((y :: t, ys), (cnt - 1, (y :: t, ys)))
			}
		}
		unfoldRight_i[(List[T], List[T]), (Int, (List[T], List[T]))](
            func, (n, (Nil, lst))).headOption.getOrElse(Nil, lst)
	}
	
	def take_u[T](n: Int, lst: List[T]): List[T] = 
		splitAt_u(n, lst)._1.reverse
	
	def drop_u[T](n: Int, lst: List[T]): List[T] = 
		splitAt_u(n, lst)._2
	
	def exists_forall_u[T](pred: (T => Boolean), lst: List[T]):
			(Boolean, Boolean) = {
		val func = (a0_a1_rst: ((Boolean, Boolean), List[T])) => 
				a0_a1_rst match { 
			case (a0_a1, rst) => (a0_a1, rst) match { 
				case (_, Nil) => None
				case ((a0, a1), y :: ys) => 
					Some((a0 || pred(y), a1 && pred(y)), 
						((a0 || pred(y), a1 && pred(y)), ys))
			}
		}
		unfoldRight_i[(Boolean, Boolean), ((Boolean, Boolean), List[T])](
            func, ((false, true), lst)).headOption.getOrElse(false, true)
	}
	
	def exists_u[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_u(pred, lst)._1
	
	def forall_u[T](pred: (T => Boolean), lst: List[T]): Boolean = 
		exists_forall_u(pred, lst)._2
	
	def map_u[T, U](proc: (T => U), lst: List[T]): List[U] = {
		val func = (rst: List[T]) => rst match { 
			case Nil => None
			case y :: ys => Some(proc(y), ys)
		}
		unfoldRight_i[U, List[T]](func, lst).reverse
	}
	
	def foreach_u[T](proc: (T => Unit), lst: List[T]): Unit = {
		val func = (rst: List[T]) => rst match { 
			case Nil => None
			case y :: ys => Some(proc(y), ys)
		}
		unfoldRight_i[Unit, List[T]](func, lst)
	}
	
	def partition_u[T](pred: (T => Boolean), lst: List[T]): 
			(List[T], List[T]) = {
		val func = (f_r_rst: ((List[T], List[T]), List[T])) => 
			f_r_rst match { case (f_r, rst) => (f_r, rst) match {
				case (_, Nil) => None
				case ((f, r), y :: ys) => pred(y) match {
					case true => Some((y :: f, r), ((y :: f, r), ys))
					case _ => Some((f, y :: r), ((f, y :: r), ys))
				}
			}
		}
		unfoldRight_i[(List[T], List[T]), ((List[T], List[T]), List[T])](
            func, ((Nil, Nil), lst.reverse)).headOption.getOrElse(Nil, lst)
	}
	
	def filter_u[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_u(pred, lst)._1
	
	def remove_u[T](pred: (T => Boolean), lst: List[T]): List[T] = 
		partition_u(pred, lst)._2
	
	def isOrdered_u[T <% Ordered[T]](coll: Iterable[T],
            isRev: Boolean = false): Boolean = {
		val func = (acc_cur_rst: (Boolean, (T, List[T]))) => 
				acc_cur_rst match { case (acc, cur_rst) => 
			(isRev, acc, cur_rst) match {
				case (_, _, (_, Nil)) => None
				case (false, acc, (y, z :: zs)) => 
					Some(acc && (y <= z), (acc && (y <= z), (z, zs)))
				case (true, acc, (y, z :: zs)) => 
					Some(acc && (y >= z), (acc && (y >= z), (z, zs)))
			}
		}
		coll.toList match {
			case Nil => true
			case x :: xs => 
				unfoldRight_i[Boolean, (Boolean, (T, List[T]))](
					func, (true, (x, xs))).headOption.getOrElse(true)
		}
	}
	
	def append_u[T](lst1: List[T], lst2: List[T]): List[T] = {
		val func = (h_t: (List[T], List[T])) => h_t match { 
			case (_, Nil) => None
			case (h, y :: ys) => Some(y :: h, (y :: h, ys))
		}
		unfoldRight_i[List[T], (List[T], List[T])](
			func, (lst2, lst1.reverse)).headOption.getOrElse(lst2)
	}
	
	def interleave_u[T](lst1: List[T], lst2: List[T]): List[T] = {
		val func = (wss_zss: (List[T], List[T])) => wss_zss match { 
			case (Nil, Nil) => None
			case (Nil, z :: zs) => Some(z, (zs, Nil))
			case (w :: ws, zss) => Some(w, (zss, ws))
		}
		unfoldRight_i[T, (List[T], List[T])](func, (lst1, lst2)).reverse
	}
	
	def map2_u[S, T, U](proc: ((S, T) => U), lst1: List[S], 
			lst2: List[T]): List[U] = {
		val func = (wss_zss: (List[S], List[T])) => wss_zss match { 
			case (Nil, _) | (_, Nil) => None
			case (w :: ws, z :: zs) => Some(proc(w, z), (ws, zs))
		}
		unfoldRight_i[U, (List[S], List[T])](func, (lst1, lst2)).reverse
	}

	def zip_u[S, T](lst1: List[S], lst2: List[T]): 
			List[(S, T)] = 
		map2_u(((e1: S, e2: T) => (e1, e2)), lst1, lst2)
	
	def unzip_u[T, U](lst: List[(T, U)]): 
			(List[T], List[U]) = {
		val func = (wss_zss_rst: ((List[T], List[U]), List[(T, U)])) => 
			wss_zss_rst match { case (wss_zss, rst) => (wss_zss, rst) match {
				case (_, Nil) => None
				case ((ah, at), y :: ys) => 
					Some((y._1 :: ah, y._2 :: at), 
					((y._1 :: ah, y._2 :: at), ys))
			}
		}
		unfoldRight_i[(List[T], List[U]), ((List[T], List[U]), List[(T, U)])](
            func, ((Nil, Nil), lst.reverse)).headOption.getOrElse(Nil, Nil)
	}
	
	def concat_u[T](nlst: List[List[T]]): List[T] = {
		val func = (acc_rst: (List[T], List[List[T]])) => acc_rst match { 
			case (_, Nil) => None
			case (acc, y :: ys) => Some(y ++ acc, (y ++ acc, ys))
		}
		unfoldRight_i[List[T], (List[T], List[List[T]])](
			func, (Nil, nlst.reverse)).headOption.getOrElse(Nil)
	}
	
	
	
	def tabulate_lc[T](func: (Int => T), cnt: Int): List[T] = 
		/*for { 
			i <- List.range(0, cnt)
		} yield func(i)*/
		List.range(0, cnt).map(i => func(i))
	
	def nth_lc[T](idx: Int, lst: List[T]): Option[T] = 
		List.range(0, lst.size).zip(lst).filter(i_e => idx == i_e._1).
			headOption match {
				case None => None
				case Some(i_e) => Some(i_e._2)
			}
	
	def index_find_lc[T](idx: Int, data: T, lst: List[T], 
			cmp: Comparator[T]): (Int, Option[T]) = 
		List.range(0, lst.size).zip(lst.drop(0)).filter(
			i_e => (0 == cmp.compare(data, i_e._2))).
			headOption match {
				case None => (-1, None)
				case Some(i_e) => (i_e._1, Some(i_e._2))
			}
	
	def indexOf_lc[T](data: T, lst: List[T], cmp: Comparator[T]): Int = 
		index_find_lc(0, data, lst, cmp)._1
	
	def find_lc[T](data: T, lst: List[T], cmp: Comparator[T]): Option[T] = 
		index_find_lc(0, data, lst, cmp)._2
	
	def copyOf_lc[T](lst: List[T]): List[T] = lst.map(identity)
	
	def take_lc[T](n: Int, lst: List[T]): List[T] = 
		List.range(0, lst.size).zip(lst).filter(i_e => n > i_e._1).map(
			i_e => i_e._2)
	
	def drop_lc[T](n: Int, lst: List[T]): List[T] = 
		List.range(0, lst.size).zip(lst).filter(i_e => !(n > i_e._1)).map(
			i_e => i_e._2)
	
	def isOrdered_lc[T <% Ordered[T]](coll: Iterable[T],
            isRev: Boolean = false): Boolean = (isRev, coll.toList) match {
		case (_, Nil) => true
		case (false, x :: xs) => (x :: xs).zip(xs).forall(
			a_b => a_b._1 <= a_b._2)
		case (true, x :: xs) => (x :: xs).zip(xs).forall(
			a_b => a_b._1 >= a_b._2)
	}
	
	def interleave_lc[T](lst1: List[T], lst2: List[T]): List[T] = 
		lst1.zip(lst2).map(e1_e2 => List[T](e1_e2._1, e1_e2._2)).flatten ++
			(lst1.drop(lst2.size) ++ lst2.drop(lst1.size))
	
	def map2_lc[S, T, U](proc: ((S, T) => U), lst1: List[S], 
			lst2: List[T]): List[U] = {
		lst1.zip(lst2).map(e1_e2 => proc(e1_e2._1, e1_e2._2))
	}

	def zip_lc[S, T](lst1: List[S], lst2: List[T]): 
			List[(S, T)] = 
		map2_lc(((e1: S, e2: T) => (e1, e2)), lst1, lst2)
}

class SequenceopsHiorder {
}

}

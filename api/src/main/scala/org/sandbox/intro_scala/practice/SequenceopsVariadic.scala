/** DocComment:
 * <p>Brief description.</p> */
package org.sandbox.intro_scala.practice {

//import scala.collection.JavaConverters._

object SequenceopsVariadic {
	/*private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
			List[T] = {
		def iter(cur: U, acc: List[T]): List[T] = func(cur) match {
			case None => acc
			case Some((a, new_cur)) =>  iter(new_cur, a :: acc)
		}
		iter(seed, Nil)
	}*/
    private def unfoldRight_i[T, U](func: (U => Option[(T, U)]), seed: U): 
        List[T] = Sequenceops.unfoldRight_i(func, seed)
    
	private def tupOfHeads[T](items: List[T]): Product with Serializable =
			items match {
		//case Nil => List[T]()
        case Nil => throw new NotImplementedError("not implemented Tuple0")
		case List(a) => Tuple1[T](a) //.asInstanceOf[Tuple1[T]]
		case List(a, b) => Tuple2[T, T](a, b)
		case List(a, b, c) => Tuple3[T, T, T](a, b, c)
		case List(a, b, c, d) => Tuple4[T, T, T, T](a, b, c, d)
		case List(a, b, c, d, e) => Tuple5[T, T, T, T, T](a, b, c, d, e)
		case List(a, b, c, d, e, f) => 
			Tuple6[T, T, T, T, T, T](a, b, c, d, e, f)
		case _ => 
			throw new NotImplementedError("not implemented beyond Tuple6")
	}
	
	def exists_forall_iv[T](pred: ((T*) => Boolean), xss: List[T]*):
			(Boolean, Boolean) = {
		def iter(a0: Boolean, a1: Boolean, rst: Seq[List[T]]):
				(Boolean, Boolean) = rst.exists(e => Nil == e) match {
			case true => (a0, a1)
			case _ => iter(a0 || pred(rst.map(e => e.head): _*),
				a1 && pred(rst.map(e => e.head): _*), rst.map(e => e.tail))
		}
		iter(false, true, xss)
	}
	
	def exists_iv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_iv(pred, xss: _*)._1
	
	def forall_iv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_iv(pred, xss: _*)._2
	
	def exists_forall_rv[T](pred: ((T*) => Boolean), xss: List[T]*):
			(Boolean, Boolean) = xss.exists(e => Nil == e) match {
		case true => (false, true)
		case _ => 
			(pred(xss.map(e => e.head): _*) || 
			exists_forall_rv(pred, xss.map(e => e.tail): _*)._1,
			pred(xss.map(e => e.head): _*) && 
			exists_forall_rv(pred, xss.map(e => e.tail): _*)._2)
	}
	
	def exists_rv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_rv(pred, xss: _*)._1
	
	def forall_rv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_rv(pred, xss: _*)._2
	
	def map_iv[T, U](proc: ((T*) => U), xss: List[T]*): List[U] = {
		def iter(rst: Seq[List[T]], acc: List[U]): List[U] =
				rst.exists(e => Nil == e) match {
			case true => acc.reverse
			case _ => iter(rst.map(e => e.tail), 
				proc(rst.map(e => e.head): _*) :: acc)
		}
		iter(xss, List[U]())
	}
	
	def map_rv[T, U](proc: ((T*) => U), xss: List[T]*): List[U] = 
			xss.exists(e => Nil == e) match {
		case true => Nil
		case _ => proc(xss.map(e => e.head): _*) :: 
			map_rv(proc, xss.map(e => e.tail): _*)
	}
	
	def foreach_iv[T](proc: ((T*) => Unit), xss: List[T]*): Unit = {
		def iter(rst: Seq[List[T]]): Unit =
				rst.exists(e => Nil == e) match {
			case true => ()
			case _ => proc(rst.map(e => e.head): _*)
				iter(rst.map(e => e.tail))
		}
		iter(xss)
	}
	
	def foreach_rv[T](proc: ((T*) => Unit), xss: List[T]*): Unit = 
			xss.exists(e => Nil == e) match {
		case true => ()
		case _ => proc(xss.map(e => e.head): _*)
			foreach_rv(proc, xss.map(e => e.tail): _*)
	}
	
	def foldLeft_iv[T, U](corp: ((U, T*) => U), init: U, xss: List[T]*): U = 
        {
		def iter(rst: Seq[List[T]], acc: U): U = 
				rst.exists(e => Nil == e) match {
			case true => acc
			case _ => iter(rst.map(e => e.tail),
				corp(acc, rst.map(e => e.head): _*))
		}
		iter(xss, init)
	}
	
	def foldLeft_rv[T, U](corp: ((U, T*) => U), init: U, xss: List[T]*): U = 
			xss.exists(e => Nil == e) match {
		case true => init
		case _ => foldLeft_rv(corp, corp(init, xss.map(e => e.head): _*), 
			xss.map(e => e.tail): _*)
	}
	
	def foldRight_iv[T, U](proc: ((U, T*) => U), init: U, xss: List[T]*): U = 
        {
		def iter(rst: Seq[List[T]], acc: U): U = 
				rst.exists(e => Nil == e) match {
			case true => acc
			case _ => iter(rst.map(e => e.tail),
				proc(acc, rst.map(e => e.head): _*))
		}
        val len_short = xss.map(e => e.size).min
		iter(xss.map(e => e.take(len_short).reverse), init)
	}
	
	def foldRight_rv[T, U](proc: ((U, T*) => U), init: U, xss: List[T]*): U =
        {
		def _helper(acc: U, rst: List[T]*): U = 
				rst.exists(e => Nil == e) match {
			case true => acc
			case _ => _helper(proc(acc, rst.map(e => e.head): _*), 
				rst.map(e => e.tail): _*)
		}
        val len_short = xss.map(e => e.size).min
		_helper(init, xss.map(e => e.take(len_short).reverse): _*)
	}
	
	def append_iv[T](xss: List[T]*): List[T] = {
		def iter(rst: Seq[List[T]], acc: List[T]): List[T] = rst match {
			case Nil => acc
			case x :: xs => iter(xs, x ++ acc)
		}
		iter(xss.reverse, List[T]())
	}
	
	def append_rv[T](xss: List[T]*): List[T] = xss match {
		case Nil => Nil
		case x :: xs => x ++ append_rv(xs: _*)
	}
	
	/*def zipTup_iv[T, U](xss: List[T]*): List[U] = 
			xss.exists(e => Nil == e) match {
		case true => List[U]()
		case _ =>
			def iter(rst: Seq[List[T]], acc: List[U]): List[U] =
					rst.exists(e => Nil == e) match {
				case true => acc.reverse
				case _ => iter(rst.map(e => e.tail), 
					tupOfHeads(rst.map(e => e.head).toList).asInstanceOf[U] :: acc)
			}
			iter(xss, List[U]())
	}*/
	
	def zip_iv[T, U](xss: List[T]*): List[U] = 
		map_iv((els: Seq[T]) => List(els: _*).asInstanceOf[U], xss: _*)
	
	def zip_rv[T, U](xss: List[T]*): List[U] = 
		map_rv((els: Seq[T]) => List(els: _*).asInstanceOf[U], xss: _*)
	
	
	
	/*def exists_forall_fv[T](pred: ((T*) => Boolean), xss: List[T]*):
			(Boolean, Boolean) = {
		def corp(a0_a1: (Boolean, Boolean), els: List[T]): (Boolean, Boolean) 
			= (a0_a1._1 || pred(els: _*), a0_a1._2 && pred(els: _*))
		xss.foldLeft((false, true))(corp)
	}*/
	
	def exists_fv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		//exists_forall_fv(pred, xss: _*)._1
		xss.foldLeft(false)((acc, els) => acc || pred(els: _*))
	
	def forall_fv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		//exists_forall_fv(pred, xss: _*)._2
		xss.foldLeft(true)((acc, els) => acc && pred(els: _*))
	
	def map_fv[T, U](proc: ((T*) => U), xss: List[T]*): List[U] = 
		SequenceopsVariadic.zip_iv[T, List[T]](xss: _*).foldRight(List[U]())(
			(els, acc) => proc(els: _*) :: acc)
	
	def foreach_fv[T](proc: ((T*) => Unit), xss: List[T]*): Unit = 
		SequenceopsVariadic.zip_iv[T, List[T]](xss: _*).foldLeft(())(
			(_, els) => proc(els: _*))
	
	def append_fv[T](xss: List[T]*): List[T] = 
		//xss.foldRight(List[T]())((e, acc) => e ++ acc)
		xss.reverse.foldLeft(List[T]())((acc, e) => e ++ acc)
	
	def zip_fv[T, U](xss: List[T]*): List[U] = 
		map_fv((els: Seq[T]) => List(els: _*).asInstanceOf[U], xss: _*)
	
	
	
	def exists_forall_uv[T](pred: ((T*) => Boolean), xss: List[T]*):
			(Boolean, Boolean) = {
		def func(a0_a1_rst: (Boolean, Boolean, Seq[List[T]])): 
				Option[((Boolean, Boolean), (Boolean, Boolean, Seq[List[T]]))] =
				a0_a1_rst match { 
			case (a0, a1, rst) => rst.exists(e => Nil == e) match {
				case true => None
				case _ => Some((a0 || pred(rst.map(e => e.head): _*), 
					a1 && pred(rst.map(e => e.head): _*)), 
					(a0 || pred(rst.map(e => e.head): _*), 
					a1 && pred(rst.map(e => e.head): _*), 
					rst.map(e => e.tail)))
			}
		}
		unfoldRight_i[(Boolean, Boolean), (Boolean, Boolean, Seq[List[T]])](
            func, (false, true, xss)).headOption.getOrElse(false, true)
	}
	
	def exists_uv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_uv(pred, xss: _*)._1
	
	def forall_uv[T](pred: ((T*) => Boolean), xss: List[T]*): Boolean = 
		exists_forall_uv(pred, xss: _*)._2
	
	def map_uv[T, U](proc: ((T*) => U), xss: List[T]*): List[U] = {
		val func = (rst: Seq[List[T]]) => rst.exists(e => Nil == e) match { 
			case true => None
			case _ => Some(proc(rst.map(e => e.head): _*), 
				rst.map(e => e.tail))
		}
		unfoldRight_i[U, Seq[List[T]]](func, xss).reverse
	}
	
	def foreach_uv[T](proc: ((T*) => Unit), xss: List[T]*): Unit = {
		val func = (rst: Seq[List[T]]) => rst.exists(e => Nil == e) match { 
			case true => None
			case _ => Some(proc(rst.map(e => e.head): _*), 
				rst.map(e => e.tail))
		}
		unfoldRight_i[Unit, Seq[List[T]]](func, xss)
	}
	
	def append_uv[T](xss: List[T]*): List[T] = {
		val func = (rst_acc: (Seq[List[T]], List[T])) => rst_acc match {
			case (rst, acc) => rst match {
				case Nil => None
				case y :: ys => Some(y ++ acc, (ys, y ++ acc))
			}
		}
		unfoldRight_i[List[T], (Seq[List[T]], List[T])](
			func, (xss.reverse, List[T]())).headOption.getOrElse(List[T]())
	}
	
	def zip_uv[T, U](xss: List[T]*): List[U] = 
		map_uv((els: Seq[T]) => List(els: _*).asInstanceOf[U], xss: _*)
    
    def main(args: Array[String]): Unit = {
        val nss = List(List(0, 1), List(2, 3), List(4, 5, 6))
        printf("appendVar(%s): %s\n", nss.mkString("[", ", ", "]"),
            append_iv(nss: _*).mkString("[", ", ", "]"))
    }
}

class SequenceopsVariadic {
}

}

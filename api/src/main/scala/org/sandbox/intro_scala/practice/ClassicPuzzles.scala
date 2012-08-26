/** DocComment:
 * <p>Brief description.</p> */
package org.sandbox.intro_scala.practice {

object ClassicPuzzles {
	def hanoi(src: Int, dest: Int, spare: Int, numDisks: Int): 
			List[(Int, Int)] = 1 > numDisks match {
		case true => Nil
		case _ => hanoi(src, spare, dest, numDisks - 1) ++ 
			List[(Int,Int)]((src, dest)) ++ 
			hanoi(spare, dest, src, numDisks - 1)
	}
	
	def nqueens(n: Int): List[List[(Int, Int)]] = {
		def threatp(coord1: (Int, Int), coord2: (Int, Int)): Boolean = 
				(coord1, coord2) match {
			case ((x1, y1), (x2, y2)) =>
				(x1 == x2) || (y1 == y2) || 
					(math.abs(x1 - x2) == math.abs(y1 - y2))
		}
		def safep(pos: (Int, Int), placedSet: List[(Int, Int)]): Boolean = 
				placedSet match {
			case Nil => true
			case x :: xs => !(threatp(pos, x)) && safep(pos, xs)	
		}
		def iter(col: Int, row: Int, placedSet: List[(Int, Int)],
				board: List[List[(Int, Int)]]): List[List[(Int, Int)]] = 
				placedSet match {
			case _ if (n - 1) < col => (placedSet :: board).reverse
			case _ if (n - 1) < row => board
			case _ if safep((col, row), placedSet) =>
				iter(col, row + 1, placedSet, iter(col + 1, 0, 
					(col, row) :: placedSet, board))
			case _ => iter(col, row + 1, placedSet, board)
		}
		iter(0, 0, Nil, Nil)
	}
	
	def nqueensGrid(numqueens: Int, answer: List[(Int, Int)]): 
			List[List[String]] = {
		val lstN = List.range(0, numqueens)
		val lstBlank = lstN.zip(lstN.map(_ => " "))
		val lstLtrs = " " :: lstN.map(x => (x + 'a'.toInt).toChar.toString)
		def mkRow(acc: List[List[String]], h_t: (Int, Int)): List[List[String]] = 
				h_t match {
			case (h, t) => (t.toString :: lstBlank.map(i_e => 
				if (i_e._1 == h) "Q" else i_e._2)) :: acc
		}
		val answer_sorted = answer.sortWith((ah_at, bh_bt) =>
			ah_at._2 < bh_bt._2)
		answer_sorted.foldLeft(List(lstLtrs))(mkRow)
	}
}

class ClassicPuzzles {
}

}

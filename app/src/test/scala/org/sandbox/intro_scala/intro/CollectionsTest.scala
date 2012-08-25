package org.sandbox.intro_scala.intro {

import scala.collection.immutable.{Queue, List, HashMap, TreeMap}
import scala.collection.mutable.{Queue => M_Queue, ListBuffer => M_List 
	, HashMap => M_HashMap, PriorityQueue => M_PriorityQueue
    }

import org.sandbox.intro_scala.util.{Library => Util}

class CollectionsTest extends UnitSpec {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 1.0e-7f
    
    val ints: Array[Integer] = Array(2, 1, 0, 4, 3)
    val floats: Array[Float] = Array(25.7f, 0.1f, 78.5f, 52.3f)
    val chars: Array[Char] = Array('a', 'e', 'k', 'p', 'u', 'k', 'a')
    
    behavior of "Collections ops test(s)"
    
    before {
		System.err.println("###setup TestCase###")
	}
	after {
		System.err.println("###teardown TestCase###")
	}
    override def beforeEach() : Unit = {
		System.err.println("setup Test ...")
	}
	override def afterEach() : Unit = {
		System.err.println("... teardown Test")
	}
	
    it should "test deques" in {
		// collection.immutable
		var queue1 = Queue[Float]()
		assertResult(true, "isEmpty") { queue1.isEmpty }
		queue1 = queue1.enqueue(floats.toList)
		assertResult(floats.length, "length") { queue1.size }
		assertResult(true, "peek") { Util.in_epsilon(epsilon, floats(0), 
			queue1.headOption.getOrElse(0.0f): Float) }
		val len_old = queue1.size
		queue1 = queue1.enqueue(-0.5f)
		assertResult(len_old + 1, "offer") { queue1.size }
		val (el, new_q) = queue1.dequeue
		queue1 = new_q
		assertResult(true, "poll") { Util.in_epsilon(epsilon, floats(0), el) }
		assertResult("[0.1, 78.5, 52.3, -0.5]", "toString")
			{ queue1.mkString("[", ", ", "]") }
		
		// collection.mutable
		val m_queue1 = M_Queue[Float]()
		assertResult(true, "isEmpty") { m_queue1.isEmpty }
		m_queue1.enqueue(floats: _*)
		assertResult(floats.length, "length") { m_queue1.size }
		assertResult(true, "peek") { Util.in_epsilon(epsilon, floats(0), 
			m_queue1.headOption.getOrElse(0.0f): Float) }
		val len_oldM = m_queue1.size
		m_queue1.enqueue(-0.5f)
		assertResult(len_oldM + 1, "offer") { m_queue1.size }
		assertResult(true, "poll") { Util.in_epsilon(epsilon, floats(0), 
            m_queue1.dequeue) }
		assertResult("[0.1, 78.5, 52.3, -0.5]", "toString")
			{ m_queue1.mkString("[", ", ", "]") }
	}
    
	it should "test lists" in {
		val nines: Array[Integer] = Array(9, 9, 9, 9)
		
		// collection.immutable
		var lst1 = List[Integer]()
		assertResult(true, "isEmpty") { lst1.isEmpty }
		lst1 = lst1 ++ ints.toList // List.concat[Integer](lst1, ints.toList)
		assertResult(ints.length, "length") { lst1.size }
		assertResult(ints(0), "first") { lst1(0) }
		assertResult(ints(2), "nth") { lst1(2) }
		assertResult(1, "indexOf") { lst1.indexOf(ints(1)) }
		lst1 = lst1 ++ nines.toList
		assertResult(nines.length + ints.length, "append") { lst1.size }
		lst1 = lst1.sortWith((e1, e2) => (e2 compareTo e1) < 0)
		assertResult("[9, 9, 9, 9, 4, 3, 2, 1, 0]", "toString")
			{ lst1.mkString("[", ", ", "]") }
		
		// collection.mutable
		var m_lst1 = M_List[Integer]()
		assertResult(true, "isEmpty") { m_lst1.isEmpty }
		m_lst1.append(ints: _*)
		assertResult(ints.length, "length") { m_lst1.size }
		assertResult(ints(0), "first") { m_lst1(0) }
		assertResult(ints(2), "nth") { m_lst1(2) }
		assertResult(1, "indexOf") { m_lst1.indexOf(ints(1)) }
		m_lst1.append(nines: _*)
		assertResult(nines.length + ints.length, "append") { m_lst1.size }
		m_lst1 = m_lst1.sortWith((e1, e2) => (e2 compareTo e1) < 0)
		assertResult("[9, 9, 9, 9, 4, 3, 2, 1, 0]", "toString")
			{ m_lst1.mkString("[", ", ", "]") }
	}
    
    it should "test maps" in {
		// collection.immutable
		var map1 = HashMap[String, Character]()
		assertResult(true, "isEmpty") { map1.isEmpty }
		for (i <- 0 until chars.length)
            map1 = map1 + ("ltr " + (i % 5) -> chars(i))
        assertResult(5, "length") { map1.size }
        map1 = map1 + ("ltr 20" -> 'Z')
        assertResult(true, "contains") { map1.contains("ltr 2") }
        assertResult('k', "find") { map1("ltr 2") }
		map1 = map1 - ("ltr 2")
		assertResult(false, "remove") { map1.contains("ltr 2") }
		
		// collection.mutable
		val m_map1 = M_HashMap[String, Character]()
		assertResult(true, "isEmpty") { m_map1.isEmpty }
		for (i <- 0 until chars.length)
            m_map1 += ("ltr " + (i % 5) -> chars(i))
        assertResult(5, "length") { m_map1.size }
        m_map1 += ("ltr 20" -> 'Z')
        assertResult(true, "contains") { m_map1.contains("ltr 2") }
        assertResult('k', "find") { m_map1("ltr 2") }
		m_map1 -= ("ltr 2")
		assertResult(false, "remove") { m_map1.contains("ltr 2") }
	}
    
    it should "test priorqs" in {
		// collection.mutable
		var floats2 = floats.map(identity)
		val pri_q1 = M_PriorityQueue[Float]()(Ordering[Float].reverse)
		val pri_q2 = M_PriorityQueue[Float]()
		assertResult(true, "isEmpty") { pri_q1.isEmpty }
		for (fNum <- floats) {
			pri_q1 += fNum
			pri_q2 += fNum
		}
		floats2 = floats.sorted
		assertResult(floats2.length, "length") { pri_q1.size }
		assertResult(true, "peek") { Util.in_epsilon(epsilon, floats2(0), 
			pri_q1.headOption.getOrElse(0.0f): Float) }
		assertResult(true, "peek (rev)") { Util.in_epsilon(epsilon,
            floats2(floats2.length - 1),
            pri_q2.headOption.getOrElse(0.0f): Float) }
		assertResult(true, "poll") { Util.in_epsilon(epsilon, floats2(0), 
            pri_q1.dequeue) }
		assertResult(true, "poll (rev)") { Util.in_epsilon(epsilon,
            floats2(floats2.length - 1), pri_q2.dequeue) }
		val (len_old1, len_old2) = (pri_q1.size, pri_q2.size)
		pri_q1.enqueue(-0.5f)
		pri_q2.enqueue(-0.5f)
		assertResult(len_old1 + 1, "offer")  { pri_q1.size }
		assertResult(len_old2 + 1, "offer (rev)") {pri_q2.size }
		assertResult("[-0.5, 25.7, 78.5, 52.3]", "toString")
			{ pri_q1.mkString("[", ", ", "]") }
		assertResult("[52.3, 0.1, 25.7, -0.5]", "toString (rev)")
			{ pri_q2.mkString("[", ", ", "]") }
	}
    
    it should "test treemaps" in {
		// collection.immutable
		var tree1 = TreeMap[String, Character]()
		assertResult(true, "isEmpty") { tree1.isEmpty }
		for (i <- 0 until chars.length)
            tree1 = tree1 + ("ltr " + (i % 5) -> chars(i))
        assertResult(5, "length") { tree1.size }
        tree1 = tree1 + ("ltr 20" -> 'Z')
        assertResult(true, "contains") { tree1.contains("ltr 2") }
        assertResult('k', "find") { tree1("ltr 2") }
		tree1 = tree1 - ("ltr 2")
		assertResult(false, "remove") { tree1.contains("ltr 2") }
	}
}

object CollectionsTest {
    
}

}

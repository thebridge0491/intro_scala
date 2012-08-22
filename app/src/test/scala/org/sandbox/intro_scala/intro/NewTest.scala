package org.sandbox.intro_scala.intro {

import org.scalatest.{time,concurrent}

import org.sandbox.intro_scala.util.{Library => Util}

class NewTest extends UnitSpec {
    //val tolerance = 2.0f * Float.MinPositiveValue
    val epsilon = 1.0e-7f
    
    behavior of "New example test(s)"
    
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
	
    it should "exist as a class" in {
		try {
			Class.forName("%s.Intro".format(
                this.getClass.getPackage.getName))
            assert(true)
		} catch {
			case exc: ClassNotFoundException => {
                //fail("Class(es) not existent: " + 
                //    classOf[Intro].getName)
                fail("%s %s".format("Class(es) not existent:", 
                    Array(classOf[Intro].getName).mkString))
            }
		}
	}
	
    it should "test method" in {
		assertResult(4) { 2 * 2 }
	}
	it should "test dblMethod" in {
		assertResult(true) { Util.in_epsilon(100.001f, 100.001f, epsilon) }
	}
    it should "test strMethod" in {
		assertResult("Hello") { "Hello" }
	}
    ignore should "test ignoredMethod" in {
		assertResult(5) { 2 * 2 }
	}
    it should "test failMethod" in {
		fail()
	}
    it should "test exceptionMethod" in {
		assertThrows[IllegalArgumentException] { 
			throw new IllegalArgumentException }
	}
	it should "test timeoutMethod" in {
		concurrent.TimeLimits.failAfter(time.Span(2000, time.Millis)) {
			for (i <- List.range(0, 1.0e6.toInt, 1)) {}
			Thread.sleep(2000)
		}
	}
}

object NewTest {
    
}

}

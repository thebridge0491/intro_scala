package org.sandbox.intro_scala.practice {

import org.scalatest.{Suite,BeforeAndAfterAll,BeforeAndAfter}

class Ts_Main extends Suite with BeforeAndAfter with BeforeAndAfterAll {
    override def beforeAll(): Unit = {
	}
	override def afterAll(): Unit = {
	}
    before {
	}
	after {
	}
	override def nestedSuites = scala.collection.immutable.IndexedSeq[Suite](
		new ClassicTest, new ClassicProp, new SequenceopsTest,
        new SequenceopsProp)
}

object Ts_Main {
    def main(args: Array[String]): Unit = {
    	if (1 > args.length) {
			org.scalatest.run(new Ts_Main)
		} else
			org.scalatest.tools.Runner.main(args)
    }
}

}

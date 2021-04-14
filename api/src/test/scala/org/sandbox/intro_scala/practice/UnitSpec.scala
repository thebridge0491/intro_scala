package org.sandbox.intro_scala.practice {

import org.scalatest._
import flatspec._
import matchers._

abstract class UnitSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfterEach with BeforeAndAfter

object Tag1 extends Tag("Tag1")
object Tag2 extends Tag("Tag2")

}

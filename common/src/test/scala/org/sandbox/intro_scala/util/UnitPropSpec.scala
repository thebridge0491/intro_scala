package org.sandbox.intro_scala.util {

import org.scalatest._
import propspec._
import matchers._

abstract class UnitPropSpec extends AnyPropSpec with prop.TableDrivenPropertyChecks with should.Matchers with BeforeAndAfterEach with BeforeAndAfter

//object Tag1 extends Tag("Tag1")
//object Tag2 extends Tag("Tag2")

}

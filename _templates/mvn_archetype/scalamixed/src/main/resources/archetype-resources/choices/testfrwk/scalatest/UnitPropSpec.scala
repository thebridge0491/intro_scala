#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.scalatest._
import propspec._
import matchers._

abstract class UnitPropSpec extends AnyPropSpec with prop.TableDrivenPropertyChecks with should.Matchers with BeforeAndAfterEach with BeforeAndAfter

//object Tag1 extends Tag("Tag1")
//object Tag2 extends Tag("Tag2")

}

#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import org.scalatest._

abstract class UnitPropSpec extends PropSpec with prop.Checkers with Matchers with BeforeAndAfterEach with BeforeAndAfter

//object Tag1 extends Tag("Tag1")
//object Tag2 extends Tag("Tag2")

}

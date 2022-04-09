#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

@org.junit.platform.suite.api.Suite
@org.junit.platform.suite.api.IncludeClassNamePatterns(Array("^.*Test${symbol_dollar}",
  "^Test.*${symbol_dollar}", "^.*Prop${symbol_dollar}", "^Prop.*${symbol_dollar}"))
@org.junit.platform.suite.api.SelectClasses(Array(classOf[NewTest], classOf[NewProp]))
//@org.junit.platform.suite.api.SelectClasses(Array(classOf[NewTest], classOf[NewProp],
//  classOf[ClassicTest], classOf[ClassicProp]))
class Ts_Main {
    @org.junit.jupiter.api.BeforeEach
    def setUp(): Unit = {
    }
    @org.junit.jupiter.api.AfterEach
    def tearDown(): Unit = {
    }
}

object Ts_Main {
    @org.junit.jupiter.api.BeforeAll
    def setUpClass(): Unit = {
    }
    @org.junit.jupiter.api.AfterAll
    def tearDownClass(): Unit = {
    }

    def main(args: Array[String]): Unit = {
        if (1 > args.length)
        	org.junit.platform.console.ConsoleLauncher.main("-c",
              classOf[NewTest].getName, "-c", classOf[NewProp].getName)
        else
            org.junit.platform.console.ConsoleLauncher.main(args :_*)
    }
}

}

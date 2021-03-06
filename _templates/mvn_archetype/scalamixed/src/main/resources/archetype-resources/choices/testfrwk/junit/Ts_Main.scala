#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

@org.junit.runner.RunWith(classOf[org.junit.runners.Suite])
@org.junit.runners.Suite.SuiteClasses(Array(classOf[NewTest], classOf[NewProp]))
//@org.junit.runners.Suite.SuiteClasses(Array(classOf[NewTest], classOf[NewProp],
//    classOf[ClassicTest], classOf[ClassicProp]))
class Ts_Main {
    @org.junit.Before
    def setUp(): Unit = {
    }
    @org.junit.After
    def tearDown(): Unit = {
    }
}

object Ts_Main {
    @org.junit.BeforeClass
    def setUpClass(): Unit = {
    }
    @org.junit.AfterClass
    def tearDownClass(): Unit = {
    }
    
    def main(args: Array[String]): Unit = {
        if (1 > args.length)
            org.junit.runner.JUnitCore.main(classOf[Ts_Main].getName)
        else {
            args.foreach { s =>
                try {
                    val cls = Class.forName(s)
                } catch {
                    case exc: ClassNotFoundException => {
                        Console.err.println(exc) //exc.printStackTrace()
                        sys.exit(1)
                    }
                }
            }
            org.junit.runner.JUnitCore.main(args :_*)
        }
    }

}

}

#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

class Ts_Main {
    @org.testng.annotations.BeforeMethod
    def setUp(): Unit = {
    }
    @org.testng.annotations.AfterMethod
    def tearDown(): Unit = {
    }
}

object Ts_Main {
    @org.testng.annotations.BeforeClass
    def setUpClass(): Unit = {
    }
    @org.testng.annotations.AfterClass
    def tearDownClass(): Unit = {
    }
    
    def main(args: Array[String]): Unit = {
    	if (1 > args.length) {
			val testng = new org.testng.TestNG()
			var suite: java.util.List[org.testng.xml.XmlSuite] = 
				new java.util.ArrayList[org.testng.xml.XmlSuite]()
			var istrm: java.io.InputStream = null
			try {
				try {
					istrm = Ts_Main.getClass.getResourceAsStream("/testng.xml")
					suite = new org.testng.xml.Parser(istrm).parseToList()
					istrm.close()
				} catch {
					case exc0: java.io.IOException => {
						try {
							suite = new org.testng.xml.Parser(
								"src/test/resources/testng.xml").parseToList()
						} catch {
							case exc1: java.io.IOException => {
								exc0.printStackTrace()
								exc1.printStackTrace()
								System.exit(1)
							}
						}
					}
				}
				testng.setOutputDirectory("build/test-output")
				testng.setXmlSuites(suite)
				testng.run()
			} catch {
				case exc: javax.xml.parsers.ParserConfigurationException => {
					exc.printStackTrace()
				}
				case exc: org.xml.sax.SAXException => {
					exc.printStackTrace()
				}
				case exc: java.io.IOException => {
					exc.printStackTrace()
				}
			}
		} else
			org.testng.TestNG.main(args)
    }
}

}

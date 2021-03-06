#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import scala.util._
import scala.collection.JavaConverters._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/*
 -- run w/out compile --
 // note: Script.scala without package declaration
 scala -nocompdaemon [-cp classes:depn0.jar] Script.scala [arg1 argN]
 
 -- run REPL, load script, & run --
 scala [-cp classes:depn0.jar] -i Script.scala
 scala> Script.main(Array(arg1, argN))
 
 -- help/info tools in REPL --
 :help, :quit, :reset, :require <jar>, :type <expr>, :kind <type>
 
 -- show module/type info --
 ??
 */

/** DocComment:
 * Brief description. <p> */
object Main {
	// from cmdln: java -Dlog4j.configurationFile=path/log4j2.xml ...
	//sys.props.put("log4j.configurationFile", "log4j2.xml")
	// from cmdln: java -Dlogback.configurationFile=path/logback.xml ...
	sys.props.put("logback.configurationFile", "logback.xml")
	
	private val rootLogger = LoggerFactory.getLogger(this.getClass.getName())
    
    private def run_${name}(progname: String, name: String): Unit = {
        //val re = new scala.util.matching.Regex("""(?i)quit""")
        val re = """(?i)quit""".r
        
        printf("%s match: %s to %s\n",
		    /*name match {
		    	case re(_*) => "Good"
		    	case _ => "Does not"
		    },*/
		    (if (None != re.findFirstMatchIn(name)) "Good" else "Does not"),
		    name, re.pattern)
    }
    
    private def printUsage(str: String) = {
        val usageStr = """
            Usage: %s [-h][-u name]
        """.format(Main.getClass.getName())
        Console.err.println(usageStr + "\n" + str)
    }
  
    private def parse_cmdopts(optsMap: Map[String, String], args: 
    		Array[String]) = {
        //if (0 == args.length)
        //    printUsage("")
        rootLogger.info("parse_cmdopts")
        
        def nextOption(map: Map[String, String], list: List[String]) : 
        		Map[String, String] = {
            list match {
				case Nil => map
				case _ => 
					list.head match {
						case "-u" => 
							nextOption(map ++ Map("name" -> list.tail.head), list.tail.tail)
						case "-h" => printUsage("") ; sys.exit(0)
						/*
						case string :: Nil | string :: opt2 :: tail if '-' == opt2(0) => 
							nextOption(map ++ Map("infile" -> string), list.tail)
						*/ 
						case option => 
							printUsage("Unknown option " + option)
							sys.exit(1)
						/*case _ => printUsage("") ; sys.exit(1)*/
					}
			}
        }
        //nextOption(optsMap, args.toList)
        nextOption(optsMap, List[String](args: _*))
    }
    
    def main(args: Array[String]): Unit = {
        val init_map = Map("name" -> "World")
        val optsMap = parse_cmdopts(init_map, args)
        
        val rsrc_path = if (None != sys.env.getOrElse("RSRC_PATH", None)) 
            sys.env.getOrElse("RSRC_PATH", None) else sys.props.getOrElse(
            "rsrcPath", "src/main/resources")
        
        var (iniStrm, jsonStrm, yamlStrm) = (null: java.io.InputStream,
			null: java.io.InputStream, null: java.io.InputStream)
        try {
			iniStrm = new java.io.FileInputStream(rsrc_path + "/prac.conf")
			jsonStrm = new java.io.FileInputStream(rsrc_path + "/prac.json")
			yamlStrm = new java.io.FileInputStream(rsrc_path + "/prac.yaml")
		} catch {
			case exc0: java.io.IOException => {
				printf("(exc: %s) Bad env var RSRC_PATH: %s\n", exc0,
                    rsrc_path)
                
				iniStrm = Main.getClass.getResourceAsStream("/prac.conf")
				jsonStrm = Main.getClass.getResourceAsStream("/prac.json")
				yamlStrm = Main.getClass.getResourceAsStream("/prac.yaml")
			}
		}
        
        val ini_cfg = new org.ini4j.Ini()
        
        var jsonobj: javax.json.JsonObject = null
        var rdr: javax.json.JsonReader = null
        try {
        	ini_cfg.load(iniStrm)
        	rdr = javax.json.Json.createReader(jsonStrm)
			jsonobj = rdr.readObject()
        } catch {
            case exc: java.io.IOException => {
                exc.printStackTrace()
                sys.exit(1)
            }
        } finally {
			rdr.close()
		}
        
        val yaml = new org.yaml.snakeyaml.Yaml()
        val yamlmap: collection.mutable.Map[String, Object] = 
			yaml.load(yamlStrm).asInstanceOf[
				java.util.HashMap[String, Object]].asScala
        
        val tup_arr = Array(
            (ini_cfg.toString(), ini_cfg.get("default").get("domain"),
                ini_cfg.get("user1").get("name"))
			, (jsonobj.toString(), jsonobj.getString("domain"),
                jsonobj.getJsonObject("user1").getString("name"))
            , (yamlmap.toString(), yamlmap.getOrElse("domain", "???"),
                yamlmap.getOrElse("user1", new java.util.HashMap[String, Object]).asInstanceOf[java.util.HashMap[String, Object]].asScala.getOrElse("name", "???"))
            )
        
        //printf("ini4j config start section: %s\n",
		//	ini_cfg.keySet.iterator.next())
        for ((cfg_txt, domain_txt, user1name_txt) <- tup_arr) {
			printf("config: %s\n", cfg_txt)
			printf("domain: %s\n", domain_txt)
			printf("user1Name: %s\n\n", user1name_txt)
		}
        
        run_${name}(Main.getClass.getName(), optsMap.getOrElse("name", "World"))
        
        rootLogger.debug("exiting main()")
    }
}

}

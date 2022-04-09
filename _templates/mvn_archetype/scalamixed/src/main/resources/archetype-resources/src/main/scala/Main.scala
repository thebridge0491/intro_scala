#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

import scala.util._
import scala.jdk.CollectionConverters._
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

    private def deserialize_str(data_str: String, fmt: String): collection.mutable.Map[String, Object] = {
        val blank_cfg = collection.mutable.Map[String, Object]("fmt" -> fmt)
        fmt match {
            case "yaml" | "json" =>
                var yaml = new org.yaml.snakeyaml.Yaml()
                val yamlmap = yaml.load(data_str).asInstanceOf[
                    java.util.HashMap[String, Object]].asScala
                blank_cfg ++ yamlmap
            case "toml" =>
                var toml = new com.moandjiezana.toml.Toml()
                val tomlmap = toml.read(data_str).toMap().asInstanceOf[
                    java.util.HashMap[String, Object]].asScala
                blank_cfg ++ tomlmap
            /*case "json" =>
                var jsonobj: javax.json.JsonObject = null
                var rdr: javax.json.JsonReader = null
                rdr = javax.json.Json.createReader(new java.io.StringReader(
                    data_str))
                jsonobj = rdr.readObject()

                val jsonmap = collection.mutable.Map[String, Object]()
                for (entryX <- jsonobj.entrySet().asInstanceOf[java.util.Set[java.util.Map.Entry[String, Object]]].asScala) {
                    if (jsonobj.getClass() != entryX.getValue().getClass())
                        jsonmap += (entryX.getKey() -> entryX.getValue())
                    else {
                        val jsonsub = entryX.getValue().asInstanceOf[
                            javax.json.JsonObject]
                        val jsonsubmap = new java.util.HashMap[String,
                            Object]().asScala
                        for (entrySub <- jsonsub.entrySet().asInstanceOf[java.util.Set[java.util.Map.Entry[String, Object]]].asScala) {
                            jsonsubmap += (entrySub.getKey() -> entrySub.getValue())
                        }
                        jsonmap += (entryX.getKey() -> jsonsubmap.asJava)
                    }
                }
                rdr.close()
                blank_cfg ++ jsonmap*/
            case option =>
                Console.err.println("Unknown fmt " + option)
                //sys.exit(1)
                blank_cfg
        }
    }

    private def run_${name}(progname: String, name: String): Unit = {
        //val re = new scala.util.matching.Regex("""(?i)quit""")
        val re = """(?i)quit""".r

        printf("%s match: %s to %s${symbol_escape}n",
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
        Console.err.println(usageStr + "${symbol_escape}n" + str)
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
        //nextOption(optsMap, List[String](args.toIndexedSeq: _*))
        nextOption(optsMap, args.toList)
    }

    def main(args: Array[String]): Unit = {
        val init_map = Map("name" -> "World")
        val optsMap = parse_cmdopts(init_map, args)

        val rsrc_path = sys.env.getOrElse("RSRC_PATH", sys.props.getOrElse(
            "rsrcPath", "src/main/resources"))

        val ini_cfg = new org.ini4j.Ini()
        try {
            ini_cfg.load(new java.io.FileReader(rsrc_path + "/prac.conf"))
        } catch {
            case exc0: java.io.IOException => {
                printf("(exc: %s) Bad env var RSRC_PATH: %s${symbol_escape}n", exc0,
                    rsrc_path)
                try {
                    ini_cfg.load(Main.getClass.getResourceAsStream(
                        "/prac.conf"))
                } catch {
                    case exc1: java.io.IOException => {
                        exc0.printStackTrace()
                        exc1.printStackTrace()
                        sys.exit(1)
                    }
                }
            }
        }

        var json_cfg: collection.mutable.Map[String, Object] = null
        var toml_cfg: collection.mutable.Map[String, Object] = null
        var yaml_cfg: collection.mutable.Map[String, Object] = null
        try {
            json_cfg = deserialize_str(new String(new java.io.FileInputStream(
                rsrc_path + "/prac.json").readAllBytes()), "json")
            toml_cfg = deserialize_str(new String(new java.io.FileInputStream(
                rsrc_path + "/prac.toml").readAllBytes()), "toml")
            yaml_cfg = deserialize_str(new String(new java.io.FileInputStream(
                rsrc_path + "/prac.yaml").readAllBytes()), "yaml")
        } catch {
            case exc0: java.io.IOException => {
                json_cfg = deserialize_str(new String(
                    Main.getClass.getResourceAsStream("/prac.json"
                    ).readAllBytes()), "json")
                toml_cfg = deserialize_str(new String(
                    Main.getClass.getResourceAsStream("/prac.toml"
                    ).readAllBytes()), "toml")
                yaml_cfg = deserialize_str(new String(
                    Main.getClass.getResourceAsStream("/prac.yaml"
                    ).readAllBytes()), "yaml")
            }
        }

        val tup_arr = Array(
            (ini_cfg.toString(), ini_cfg.get("default").get("domain"),
                ini_cfg.get("user1").get("name"))
            , (json_cfg.toString(), json_cfg.getOrElse("domain", "???"),
                json_cfg.getOrElse("user1", new java.util.HashMap[String, Object]).asInstanceOf[java.util.HashMap[String, Object]].asScala.getOrElse("name", "???"))
            , (toml_cfg.toString(), toml_cfg.getOrElse("domain", "???"),
                toml_cfg.getOrElse("user1", new java.util.HashMap[String, Object]).asInstanceOf[java.util.HashMap[String, Object]].asScala.getOrElse("name", "???"))
            , (yaml_cfg.toString(), yaml_cfg.getOrElse("domain", "???"),
                yaml_cfg.getOrElse("user1", new java.util.HashMap[String, Object]).asInstanceOf[java.util.HashMap[String, Object]].asScala.getOrElse("name", "???"))
            )

        //printf("ini4j config start section: %s${symbol_escape}n",
		//	ini_cfg.keySet.iterator.next())
        for ((cfg_txt, domain_txt, user1name_txt) <- tup_arr) {
			printf("config: %s${symbol_escape}n", cfg_txt)
			printf("domain: %s${symbol_escape}n", domain_txt)
			printf("user1Name: %s${symbol_escape}n${symbol_escape}n", user1name_txt)
		}

        run_${name}(Main.getClass.getName(), optsMap.getOrElse("name", "World"))

        rootLogger.debug("exiting main()")
    }
}

}

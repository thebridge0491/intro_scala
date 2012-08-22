package org.sandbox.intro_scala.intro {

import scala.util._
import scala.collection.JavaConverters._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.Date
import scala.collection.mutable

import org.sandbox.intro_scala.util.{Library => Util}
import org.sandbox.intro_scala.practice._

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

object ConstItems extends Enumeration {
    type ConstItems = Value
    val Zero = Value(0)
    val NumZ = Value(26)
}

/** DocComment:
 * Brief description. <p> */
object Main {
	// from cmdln: java -Dlog4j.configurationFile=path/log4j2.xml ...
	//sys.props.put("log4j.configurationFile", "log4j2.xml")
	// from cmdln: java -Dlogback.configurationFile=path/logback.xml ...
	sys.props.put("logback.configurationFile", "logback.xml")
	
	private val rootLogger = LoggerFactory.getLogger(this.getClass.getName())
    
    private def run_intro(progname: String, rsrc_path: String, name: String,
            num: Int, is_expt2: Boolean): Unit = {
        val timeIn_mSecs: Long = System.currentTimeMillis()
        
        // basic datatypes
        var isDone = false
        var (numI, arrLen) = (0, ConstItems.Zero.id)
        val (seedi, delayMSecs) = (timeIn_mSecs.asInstanceOf[Int],
                2.5e3.asInstanceOf[Int])
        var timeDiff = 0.0f
        var ch = '\u0000'        
        
        // string & arrays
        val (noname, greetFile) = ("World", "greet.txt")
        val str = new Array[Char](64)
        val numArr = Array(9, 9, 0x9, 9) // (bin, oct, hex, dec)
        
        // composites
        val rnd = new Random(seedi)
        val user1 = new User()		// new User(name, num)
        user1.setName(name)
        user1.setNum(if (0 == num) rnd.nextInt(18) + 2 else num)
        user1.setTimeIn(timeIn_mSecs)
        
        arrLen = numArr.length
        
        for (ival: Int <- numArr)
            numI += ival
        
        assert((arrLen * numArr(0)) == numI)
        
        ch = Intro.delay_char(delayMSecs)
        
        do {
            val i_sh: Short = -1
            val i_l: Long = 1L
            val (d1, d2) = (100.0, 1.0e6)
            val tup1 = ("i_sh", -1)		// Tuple2("i_sh", -1)
            
            assert(i_sh == tup1._2)
            isDone = true
        } while (!isDone)
        
        //val re = new scala.util.matching.Regex("""(?i)quit""")
        val re = """(?i)quit""".r
        
        printf("%s match: %s to %s\n",
		    /*name match {
		    	case re(_*) => "Good"
		    	case _ => "Does not"
		    },*/
		    (if (None != re.findFirstMatchIn(name)) "Good" else "Does not"),
		    name, re.pattern)
        
        val dt1 = new Date(user1.getTimeIn())
        val greetStr = Intro.greeting(rsrc_path, greetFile, user1.getName())
        printf("%s\n%s!\n", dt1.toString(), greetStr)
        
        timeDiff = (System.currentTimeMillis() - user1.getTimeIn()) / 1000.0f
        printf("(program %s) Took %.1f seconds.\n", this.getClass().getName(), 
                timeDiff)
        println("-" * 40)
        
        val ints = Array[Integer](2, 1, 0, 4, 3)
        val lst = mutable.ListBuffer[Integer](ints: _*)
        
        if (is_expt2) {
		    printf("expt(2.0, %.1f) = %.1f\n", 
		    	user1.getNum().asInstanceOf[Float],
		    	Classic.expt_i(2.0f, user1.getNum().asInstanceOf[Float]))
		    
		    val res0 = Util.mkString[Integer](lst.asJava)
		    printf("reverse(%s): ", res0)
		    println(Util.mkString[Integer](
		    	Sequenceops.reverse_i[Integer](lst).toList.asJava))
		    
		    printf("%s.sorted: ", res0)
		    println(Util.mkString[Integer](lst.sorted.asJava))
        } else {
		    printf("fact(%d) = %d\n", user1.getNum(),
		    	Classic.fact_i(user1.getNum()))
		   	
		   	val res0 = Util.mkString[Integer](lst.asJava)
		   	val el: Integer = 3
		   	val idx = Sequenceops.indexOf_lp[Integer](el, lst.asJava, 
				Util.intCmp)
		   	printf("indexOf(%d, %s, intCmp): %d\n", el, res0, idx)
		   	
		   	val new_val: Integer = 50
		   	printf("%s += %d: ", res0, new_val)
		   	lst += new_val
		   	println(Util.mkString[Integer](lst.asJava))
        }
        println("-" * 40)
        
        val pers = new Person("I.M. Computer", 32)
        assert(pers.isInstanceOf[AnyRef]) 
									// org.sandbox.intro_scala.intro.Person
        
        try {
            assert(pers.getClass() == Class.forName(
				"org.sandbox.intro_scala.intro.Person"))
        } catch {
            case exc: ClassNotFoundException => {
                exc.printStackTrace()
            }
        }
        printf("%s\n", pers.toString)
        pers.age = 33
        printf("pers.age = 33: %s\n", pers.toString)
    }
    
    private def printUsage(str: String) = {
        val usageStr = """
            Usage: %s [-h][-u name][-n num][-2]
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
						case "-n" => 
							nextOption(map ++ Map("num" -> list.tail.head), list.tail.tail)
						case "-2" => 
							nextOption(map ++ Map("is_expt2" -> "1"), list.tail)
						case "-h" => printUsage("") ; sys.exit(0)
						case option => 
							printUsage("Unknown option " + option)
							sys.exit(1)
					}
			}
        }
        //nextOption(optsMap, args.toList)
        nextOption(optsMap, List[String](args: _*))
    }
    
    def main(args: Array[String]): Unit = {
        val init_map = Map("name" -> "World", "num" -> "0", "is_expt2" -> "")
        val optsMap = parse_cmdopts(init_map, args)
        
        val rsrc_path = if ("" != sys.env.getOrElse("RSRC_PATH", "")) 
            sys.env.getOrElse("RSRC_PATH", "") else sys.props.getOrElse(
            "rsrcPath", "src/main/resources")
        
        val ini_cfg = new org.ini4j.Ini()
        try {
        	ini_cfg.load(new java.io.FileReader(rsrc_path + "/prac.conf"))
        } catch {
            case exc0: java.io.IOException => {
                printf("(exc: %s) Bad env var RSRC_PATH: %s\n", exc0,
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
        printf("ini4j config start section: %s\n",
			ini_cfg.keySet.iterator.next())
        
        
        /*var jsonobj: javax.json.JsonObject = null
        var rdr: javax.json.JsonReader = null
        try {
			rdr = javax.json.Json.createReader(new java.io.FileReader(
				rsrc_path + "/prac.json"))
			jsonobj = rdr.readObject()
		} catch {
			case exc0: java.io.IOException => {
				rdr = javax.json.Json.createReader(
					Main.getClass.getResourceAsStream("/prac.json"))
				jsonobj = rdr.readObject()
			}
		} finally {
			rdr.close()
		}*/
        
        /*
        var yaml = new org.yaml.snakeyaml.Yaml()
        var yamlmap: collection.mutable.Map[String, Object] = null
        try {
            yamlmap = yaml.load(new java.io.FileReader(
                rsrc_path + "/prac.yaml")).
                asInstanceOf[java.util.HashMap[String, Object]].asScala
        } catch {
            case exc0: java.io.IOException => {
                yamlmap = yaml.load(Main.getClass.getResourceAsStream(
                    "/prac.yaml")).
                    asInstanceOf[java.util.HashMap[String, Object]].asScala
            }
        }
        */
        
        val tup_arr = Array(
            (ini_cfg.toString(), ini_cfg.get("default").get("domain"),
                ini_cfg.get("user1").get("name"))
            /*, (yamlmap.toString(), yamlmap.getOrElse("domain", "???"),
                yamlmap.getOrElse("user1", new java.util.HashMap[String, Object]).asInstanceOf[java.util.HashMap[String, Object]].asScala.getOrElse("name", "???"))
			, (jsonobj.toString(), jsonobj.getString("domain"),
                jsonobj.getJsonObject("user1").getString("name"))*/
            )
        
        for ((cfg_txt, domain_txt, user1name_txt) <- tup_arr) {
			printf("config: %s\n", cfg_txt)
			printf("domain: %s\n", domain_txt)
			printf("user1Name: %s\n\n", user1name_txt)
		}
        
        run_intro(Main.getClass.getName(), rsrc_path,
            optsMap.getOrElse("name", "World"),
    		optsMap.getOrElse("num", "0").toInt, 
    		optsMap.getOrElse("is_expt2", "").equals("1"))
        
        rootLogger.debug("exiting main()")
    }
}

}

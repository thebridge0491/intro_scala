/** DocComment:
 * Brief description. <p> */
package org.sandbox.intro_scala.intro {

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object Intro {
	private val pracLogger = LoggerFactory.getLogger("prac")
	
	def greeting(rsrc_path: String, greetFile: String, name: String):
            String = {
		var buf = "HELP"
		var istrm: java.io.InputStream = null
		pracLogger.info("greeting()")
        
        try {
            istrm = new java.io.FileInputStream(new java.io.File(rsrc_path + "/" + 
                greetFile))
        } catch {
            case exc0: java.io.IOException => {
                printf("(exc: %s) Bad env var RSRC_PATH: %s\n", exc0,
                    rsrc_path)
                try {
                    istrm = Intro.getClass.getResourceAsStream("/" + greetFile)
                } catch {
                    case exc1: Exception => {
                        exc0.printStackTrace()
                        exc1.printStackTrace()
                    }
                }
            }
        }
        try {
            assert(null != istrm)
            //val itr_lines = scala.io.Source.fromFile(rsrc_path + "/" +
	        //	greetFile).getLines();
            val itr_lines = new scala.io.BufferedSource(istrm).getLines()
            buf = itr_lines.next()
            istrm.close()
        } catch {
            case exc: Exception => {
                exc.printStackTrace()
            }
        }
		buf + name
	}
	
	def delay_char(msecs: Int): Char = {
		var (ch, is_looping) = ('\u0000', true)
		
        while (is_looping) {
            try {
                Thread.sleep(msecs)
            } catch {
                case exc: InterruptedException => {
                    exc.printStackTrace()
                }
            }
            print("Type any character when ready.")
            ch = Console.in.read.toChar
            
            if ('\n' == ch || '\u0000' == ch)
                is_looping = true
            else
                is_looping = false
        }
		ch
	}
    
    def main(args: Array[String]): Unit = {
        val ch = delay_char(3000)
        printf("delay_char(%d)\n", 3000)
    }
}

class Intro {
}

}

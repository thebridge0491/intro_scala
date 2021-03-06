package org.sandbox.intro_scala.foreignc {

object Classic {
    //sys.props.getOrElse("java.library.path", ".:/usr/local/lib").split(":").
    //        foreach { pathX =>
    //    com.sun.jna.NativeLibrary.addSearchPath("intro_c-practice", pathX) }
    sys.props("jna.library.path") = sys.props.getOrElse("jna.library.path",
        sys.props.getOrElse("java.library.path", ".:/usr/local/lib"))
    
    val cjna = TClassic_c.cjna
    
    def fact_i(n: Long): Long = {
        cjna.fact_i(n)
    }
    
    def fact_lp(n: Long): Long = {
        cjna.fact_lp(n)
    }
    
    def expt_i(b: Float, n: Float): Float = {
        cjna.expt_i(b, n)
    }
    
    def expt_lp(b: Float, n: Float): Float = {
        cjna.expt_lp(b, n)
    }
    
    def main(args: Array[String]): Unit = {
        printf("fact(%d): %d\n", 5, fact_i(5))
    }
}

class Classic {

}

}

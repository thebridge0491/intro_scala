#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

object Classic {
	// env LD_LIBRARY_PATH=.:/usr/local/lib
	// or -Djava.library.path=".:/usr/local/lib"
    System.loadLibrary("${artifactId}_stubs")
    
    def fact_i(n: Long): Long = {
        Classic_c.fact_i(n.toInt)
    }
    
    def fact_lp(n: Long): Long = {
        Classic_c.fact_lp(n.toInt)
    }
    
    def expt_i(b: Float, n: Float): Float = {
        Classic_c.expt_i(b, n)
    }
    
    def expt_lp(b: Float, n: Float): Float = {
        Classic_c.expt_lp(b, n)
    }
    
    def main(args: Array[String]): Unit = {
        printf("fact(%d): %d\n", 5, fact_i(5))
    }
}

class Classic {

}

}

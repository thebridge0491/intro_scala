package org.sandbox.intro_scala.foreignc {

import com.sun.jna.Native

trait TClassic_c extends com.sun.jna.Library {
	def fact_i(n: Long): Long
	def fact_lp(n: Long): Long
	
	def expt_i(b: Float, n: Float): Float
	def expt_lp(b: Float, n: Float): Float
}

object TClassic_c {
	// env LD_LIBRARY_PATH=.:/usr/local/lib
    // or
	/* // -D[java | jna].library.path=".:/usr/local/lib"
    // inside object file
    //sys.props.getOrElse("java.library.path", ".:/usr/local/lib").split(":").
    //        foreach { pathX =>
    //    com.sun.jna.NativeLibrary.addSearchPath("intro_c-practice", pathX) }
    sys.props("jna.library.path") = sys.props.getOrElse("jna.library.path",
        sys.props.getOrElse("java.library.path", ".:/usr/local/lib"))
    */
    val cjna = Native.load("intro_c-practice",
		classOf[TClassic_c]).asInstanceOf[TClassic_c]
}

}

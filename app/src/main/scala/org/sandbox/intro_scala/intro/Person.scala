package org.sandbox.intro_scala.intro {

import org.slf4j.Logger
import org.slf4j.LoggerFactory

/** DocComment:
 * Brief description. <p> */
class Person(var name: String = "ToDo", var age: Int = 10) {
	private val pracLogger = LoggerFactory.getLogger("prac")
	pracLogger.info("Person()")
    
    def this() = {
    	this("ToDo", 10)
    	pracLogger.info("Person()")
    }
    
    private def equalObjects(other: Person): Boolean = {
        ((this.name.equals(other.name) || this.name == other.name) &&
            this.age == other.age //&&
            //Double.doubleToLongBits(this.dblNum) ==
            //        Double.doubleToLongBits(other.dblNum) &&
            //Arrays.equals(this.intArr, other.intArr)
        )
    }
    
    override
    def equals(obj: Any): Boolean = obj match {
        case that: Person => that.isInstanceOf[Person] && equalObjects(that)
        case _ => false
    }
    
    override
    def hashCode(): Int = {
        val (primeMul, primeAdd) = (31, 17)
        //val d_Bits: Long = Double.doubleToLongBits(this.dblNum)
        primeMul * (
            //primeMul * (
            //    primeMul * (primeAdd + Arrays.hashCode(this.intArr)) +
            //    primeAdd + (d_Bits ^ (d_Bits >>> 32)).asInstanceOf[Int]) +
            primeAdd + this.age) +
        primeAdd + (if (null != this.name) this.name.hashCode() else 0)
    }
    
    override
    def toString(): String = {
        "%s{name=%s, age=%d}".format(this.getClass().getSimpleName(),
                this.name, this.age)
    }
}
}

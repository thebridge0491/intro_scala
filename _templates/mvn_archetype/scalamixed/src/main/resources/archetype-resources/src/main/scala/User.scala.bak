#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package} {

/** DocComment:
 * Brief description. <p> */
class User(var name: String, var num: Int = 5, var timeIn: Long = 0) {
    
    def this() {
    	this("World")
    }
    
    private def equalObjects(other: User): Boolean = {
        ((this.name.equals(other.name) || this.name == other.name) &&
            this.num == other.num && this.timeIn == other.timeIn //&&
            //Double.doubleToLongBits(this.dblNum) ==
            //        Double.doubleToLongBits(other.dblNum) &&
            //Arrays.equals(this.intArr, other.intArr)
        )
    }
    
    override
    def equals(obj: Any): Boolean = obj match {
        case that: User => that.isInstanceOf[User] && equalObjects(that)
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
            primeAdd + this.num + this.timeIn.asInstanceOf[Int]) +
        primeAdd + (if (null != this.name) this.name.hashCode() else 0)
    }
    
    override
    def toString(): String = {
        "%s{name=%s, num=%d, timeIn=%d}".format(this.getClass().getSimpleName(),
                this.name, this.num, this.timeIn)
    }
}

}

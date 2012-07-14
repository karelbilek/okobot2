package cz.okoun.okobot

class Identita(val jmeno:String, val ikonka:Pair[Int, String]) {
    override def equals(x:Any) = {
        x match {
            case p:Identita=>p.jmeno==jmeno
            case _ => false
        }
    }
    override lazy val hashCode = jmeno.hashCode
    
}

object  Identita {
    val anonym = new Identita("anonym", (0,""))
    def apply(jmeno:String, ikonkaNum:Int, ikonkaStr:String) =
        new Identita(jmeno, (ikonkaNum, ikonkaStr))
}
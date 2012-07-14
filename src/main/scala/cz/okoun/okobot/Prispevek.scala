package cz.okoun.okobot

import org.joda.time.DateTime

class Prispevek(
    val klub:Klubik, 
    val id:Int, 
    identitaMaybe:Option[Identita], //<-bez toho == anonym 
    val titulek:Option[String], //<-titulek mit nemusi 
    val telo:String, 
    val datum:DateTime, 
    val novyk:Boolean,
    
    //ID na co odpovidam
    val ancestorID:Option[Int]) {
        
        def ancestor(map:Map[Int, Prispevek]):Option[Prispevek] = {
            map.get(ancestorID.getOrElse(0))
        }

        def identita = identitaMaybe.getOrElse(Identita.anonym)
        
        override def equals(x:Any) = {
            x match {
                case p:Prispevek=>p.id==id
                case _ => false
            }
        }
        override lazy val hashCode = id.hashCode
}

/*class KlubikIterator( Iterator[Prispevek] ) extends Iterator[Prispevek] (
    
)*/


object Prispevek {

    val mesice:Map[String, Int] = Iterable(("leden",1), ("únor",2), ("březen",3), ("duben",4), ("květen",5), ("červen",6), ("červenec",7), ("srpen",8), ("září",9), ("říjen",10), ("listopad",11), ("prosinec",12)).toMap

    val ikonkaRegex = """(?s)a href="/msgbox.do?[^"]*"><img src="/ico/[^\?]*\?l=([^&]*)&i=([0-9]*)""".r

    val identitaRegex = """(?s)<span class="user">(.*?)</span>""".r

    val newRegex = """^new""".r

    val dateRegex = """(?s)class="date">([0-9]*)\.([^ \.]*) ([0-9]*) ([0-9]*):([0-9]*):([0-9]*)""".r

    val titleRegex = """(?s)<div class="title">([^<]*)</div>""".r

    val bodyRegex = """(?s)<div class="content yui-base">(.*)</div>""".r


    def apply(klub:Klubik, id:Int, identitaJmeno:String, ikonka:Int, ikonkaStr:String,titulek:String, telo:String, datum:DateTime, ancestorID:Int) = {
        
        val identita = if (identitaJmeno==Identita.anonym.jmeno) {
            None
        } else {
            Some(Identita(identitaJmeno, ikonka, ikonkaStr))
        }
        
        val ancestor = if (ancestorID==0){
            None
        } else {
            Some(ancestorID)
        }
        val mTitulek = if (titulek=="") {
            None
        } else {
            Some(titulek)
        }
        
        new Prispevek(klub, id, identita, mTitulek, telo, datum, false, ancestor)
    }

    def apply(articleID:Int, articleMess:String, ancestorID:Option[Int], klubik:Klubik):Prispevek = {
        
        val maybeIkonkaMatch = ikonkaRegex.findFirstMatchIn(articleMess)
        
        val maybeIkonka:Option[Pair[Int,String]] = if (maybeIkonkaMatch.isDefined) {
            Some((maybeIkonkaMatch.get.group(2).toInt, maybeIkonkaMatch.get.group(1)))
        } else {
            None
        }
        
        val maybeIdentitaMatch = identitaRegex.findFirstMatchIn(articleMess)

        val identita:Option[Identita]= if (maybeIdentitaMatch.isDefined){
            val author_maybe = maybeIdentitaMatch.get.group(1)
            if (author_maybe.contains("<!-- anonym -->")) {
                None
            } else {
                Some(new Identita(author_maybe, maybeIkonka.get))
            }
        } else {
            None
        }
        
        val novyk = if (newRegex.findFirstMatchIn(articleMess).isDefined) {
            true
        } else {
            false
        }

        val timeMatch = dateRegex.findFirstMatchIn(articleMess).get
        val day = timeMatch.group(1).toInt
        val month = mesice(timeMatch.group(2))
        val year = timeMatch.group(3).toInt
        val hour = timeMatch.group(4).toInt
        val minute = timeMatch.group(5).toInt
        val second = timeMatch.group(6).toInt
        val jotime = new DateTime(year, month, day, hour, minute,second)
        
        val body = bodyRegex.findFirstMatchIn(articleMess).get.group(1)
        
        val titleMaybe = titleRegex.findFirstMatchIn(articleMess)
        val title = if (titleMaybe.isDefined){Some(titleMaybe.get.group(1))} else {None}
 
        new Prispevek(klubik, articleID, identita, title, body, jotime, novyk,ancestorID)
        
    }

        
}

package cz.okoun.okobot
import org.apache.http.impl.client.DefaultHttpClient;
import cz.okoun.okobot.gaestuff._

case class KlubikInfo(klub:Klubik, 
    nestarsiNove:Option[String], 
         //none-neni novy, ""-je novy, na strance 1, "2008-blabla" - je novy na te strance
    
    isBooked:Boolean)
    
case object WrongPasswordException extends Exception
class BaseBot (val loginInfo:Option[LoginInformation]) {
    
                                    //s=0 proto, aby novy bot byl skutecne nova identita
                                    //jinak to blbne a motaji se dohromady :/
    //var cookies:Map[String,String] = Map[String,String](("s","0"))
    
    val client = new  DefaultHttpClient(new ESSXConManager())
    
    var first = true
    
    def readHTTP(url:String, 
                 httptype:HTTPType=Get,
                 keyvalues:KeyValues=KeyValues()):Option[String] = {
            
            /* if (first) {
                 first = false;
                 readHTTP("http://www.okoun.cz/")                 
             } 
                     
             def toString(kuk:Map[String,String]) = {kuk.map{case(k,v)=>k+"="+v}.reduceLeftOption{_+","+_}.getOrElse("")}
             
             println("Pred cookies: "+toString(cookies))
            
             val r:Pair[Option[String], Map[String,String]] = SimplifiedHTTP.read(url, httptype, keyvalues, cookies)
             println("po cookies: "+toString(cookies))
             
             cookies ++= r._2
             r._1*/
             SimplifiedHTTP.read(url, httptype, keyvalues, client)
    }

        //first, to catch the cookies or something like that

    
    def login:Boolean = {
        if (loginInfo.isDefined) {
            
            
            val c = readHTTP("http://www.okoun.cz/old/index.jsp", 
                Post, 
                KeyValues(("login", loginInfo.get.username),
                          ("password", loginInfo.get.password),
                          ("doLogin", "1"),
                          ("topicID", "1"))).getOrElse("")
                                
                c.contains("vitaj!") //<--lol hack
        } else {
            //anonym is always logged in :)
            true
        }
   }

   if (!login) {
     throw WrongPasswordException
   }
   
   
   //book() odnovi nove prispevky
   //slo by udelat i pres vyhledavani, ale jsem liny
   def book(k:Klubik) {
       val tukan = k.getTukan(this,Book)
       readHTTP("http://www.okoun.cz/markFavouriteBoards.do",
                Post, KeyValues(("shownBoardId("+k.id+")", "false"),
                                ("favouriteBoardId("+k.id+")", "on"),
                                ("boardId", k.id.toString),
                                ("forwardOnSuccess", "board"),
                                ("tukan", tukan)))
   }
   
   def odnov(k:Klubik) {
       //precte klub a zahodi vysledky
       k.vsechnyPrispevky(this)
   }

   private def getKlubikInfos(url:String, kv:KeyValues) = {
       val mayPage = readHTTP("http://www.okoun.cz/favourites.jsp",
                                Get, kv)
        
       val page = try {
           mayPage.get
       } catch {
           case e:Exception => println("HOLY SHIT!");println(kv); throw NecitelnaVecException
       }
                                
       BaseBot.favRegex.findAllIn(page).matchData.map{m=>
           val klubik = Klubik(m.group(1).toInt, m.group(3))
           val newInfo = m.group(4)
           val nejstarsiNoveMatch = BaseBot.noveRegex.findFirstMatchIn(newInfo)
           val nejstarsiNove :Option[String] = if (!nejstarsiNoveMatch.isDefined) {
               None
           } else {
               val pageRegexMatch = BaseBot.pageRegex.findFirstMatchIn(nejstarsiNoveMatch.get.group(1))
               if (pageRegexMatch == None) {
                   Some("")
               } else {
                   Some(pageRegexMatch.get.group(1))
               }
           }
           val bookedInfo = m.group(2)
           val bookedIs = bookedInfo == "true"
           KlubikInfo(klubik, nejstarsiNove, bookedIs)
       }.toIterable
   }

   def search(term:String):Iterable[KlubikInfo] = 
       getKlubikInfos("http://www.okoun.cz/searchBoards.do", KeyValues(("keyword", term)))      
   
   
   private def getFavorites(kv:KeyValues):Iterable[KlubikInfo] = 
         getKlubikInfos("http://www.okoun.cz/favourites.jsp",kv)
  
   def getAllFavourites:Iterable[KlubikInfo] = getFavorites(KeyValues())

   def getNewFavourites:Iterable[KlubikInfo] = getFavorites(KeyValues(("new","1")))

        //neodnovi
   def hasBooked(klubik:Klubik) = klubik.isBooked(this)
    
        //odnovi :(
   def canRead(klubik:Klubik) = klubik.canRead(this)
   def canWrite(klubik:Klubik) = klubik.canWrite(this)
   

   
}

object BaseBot {
    
    
    val favRegex = """(?s)<input type="hidden" name="shownBoardId\(([0-9]*)\)" value="([^"]*).*?<a class="name" href="/boards/([^"]*)">(.*?)<div""".r
    val noveRegex = """<b><a href="[^\?"](.*)">""".r
    val pageRegex = """\?[^f]*f=(.*)""".r
    def apply():BaseBot = new BaseBot(None)
    def apply(l:LoginInformation):BaseBot = new BaseBot(Some(l))
    
    def apply(name:String, pass:String) = new BaseBot(Some(LoginInformation(name,pass)))
    
    def main(wtf:Array[String]) {
        val bot = BaseBot("bot_almighty", "SUPERTAJNEHESLO")
        (1 to 5852).foreach {n=>
            print(n)
            val kl = Klubik(n)
            if (kl.existuje) {
                println(" - je "+kl.prettyName)
                bot.book(kl)
            } else {
                println(" - neny")
            }
        }        
    }
}


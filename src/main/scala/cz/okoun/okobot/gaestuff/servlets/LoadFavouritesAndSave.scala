package cz.okoun.okobot.gaestuff.servlets

import cz.okoun.okobot.importantFinder._
import cz.okoun.okobot._
import cz.okoun.okobot.gaestuff._
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse
import com.google.appengine.api.LifecycleManager
import com.google.appengine.api.LifecycleManager.ShutdownHook
import org.joda.time._

import cz.okoun.okobot.NecitelnaVecException

class LoadFavouritesAndSave extends HttpServlet {
    override def doGet(req: HttpServletRequest, resp: HttpServletResponse)  {
        LifecycleManager.getInstance().setShutdownHook(new ShutdownHook() {
          override def shutdown() {
            LifecycleManager.getInstance().interruptAllRequests();
          }
        });
        resp.setContentType("text/plain; charset=utf-8")
        resp.setCharacterEncoding("UTF-8")
        println("HELLO")
		def myprintln(x:Any) = {
		    println(x)
		    resp.getWriter.println(x)
	    }
	    println("jdu se pokusit nacist")
        try10times()
        val all = GAEPrispevek.loadStareNDni(7)
        (Array(1,2,3,4,5,6,7)).foreach {
            dni=>
                val datum = new DateTime().withZone(DateTimeZone.forID("Europe/Prague")).minusDays(dni)
                val older = all.filter{_.datum.isAfter(datum)}
                val nekrizek = older.filter{
                    p=> (!(p.klub.prettyName.startsWith("křížkov") || p.klub.prettyName.startsWith("Křížkov") || p.klub.prettyName.startsWith("Křížek")))
                }
                
                val ktisku = ImportantFinder.allImportant(nekrizek)
                GAECompressed.saveToGAE(dni, GenerateHTML.vygeneruj(dni, ktisku))
                
        }
        println("dan")
    }
    
    def try10times() {
        (1 to 10).foreach{_=>
            try {
                loadAndSave()
                println("hotovo")
                return
            } catch {
                case NecitelnaVecException=> println("shucks")
                case WrongPasswordException => println("rong pasport")
                case e:Exception => println("wtf"); throw e 
            }
        }
    }
    
    def loadAndSave() = {
	    println("loadAndSave uvod")
        val loginInfo = GAELoginInformation.loadFromGAE("bot_almighty").get
        println("ulozeno")
        
        val bot = BaseBot(loginInfo)
        println("bot vytvoren")
        val fav = bot.getNewFavourites
        println("klubiku je "+fav.size)
        fav.foreach {
            case KlubikInfo(klub, nejstarsiNove, _) =>
                println("Klubik "+klub.name)
                println("nn "+nejstarsiNove)
                klub.saveToGAE
                val prispevky = klub.vsechnyNove(bot, nejstarsiNove)
                println("novych je "+prispevky.size)
                prispevky.foreach {prisp=>
                    println("dalsi prispevek k ulozeni")
                    prisp.saveToGAE
                }
        }
    }
}

object LoadFavouritesAndSave {
    def main(a:Array[String]) {
        new LoadFavouritesAndSave().loadAndSave()
    }
}
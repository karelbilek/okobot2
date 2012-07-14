package cz.okoun.okobot
import SimplifiedHTTP._

import scala.annotation.tailrec
import org.joda.time._


sealed abstract class TukanType
case object Book extends TukanType
//TODO: asi nejake replies

case object NecitelnaVecException extends Exception


//interni trida
class VsechnyPrispevkyNaStrance (val prispevky:Iterator[Prispevek], val nextPage:Option[String])

class Klubik(maybeId:Option[Int], maybeName:Option[String], maybePretty:Option[String]=None) {
    
    if (maybeId == None && maybeName==None) {
        throw new IllegalArgumentException("Wrong password.")        
    }
    
    var tukans:Map[BaseBot, String] = Map[BaseBot, String]()
    
    
    import Klubik.easyReg
    lazy val name:String = if (maybeName!=None) {
        maybeName.get
    }else{  
        easyReg(Klubik.nazevRegex, refContent)
    }
    
    lazy val id:Int = if (maybeId!=None) {
        maybeId.get
    }else{       
        easyReg(Klubik.idRegex, refContent).toInt
    }
    
    lazy val prettyName:String = if (maybePretty.isDefined) {
        maybePretty.get
    } else {
        easyReg(Klubik.prettyRegex, refContent)
    }
    
    override def equals(x:Any) = {
        x match {
            case k:Klubik=>k.id==id
            case _ => false
        }
    }
    override lazy val hashCode = id.hashCode
    
    
    
    def getTukan(bot:BaseBot,tukantype:TukanType):String = {
        tukantype match {
            case Book =>
                val c = content(bot)
                Klubik.bookTukanRegex.findFirstMatchIn(c).get.group(1)
        }
    }
    

    //pro zjistovani jmena/ID/hezkeho jmena
    lazy val refContent:String=content(Klubik.basebot)
    
    def content(bb:BaseBot, kv:KeyValues=KeyValues()):String = (if (maybeName==None) {
            bb.readHTTP("http://okoun.cz/boards",
                         Get,
                         KeyValues(("boardId", id.toString))++kv)
        } else {
            bb.readHTTP("http://okoun.cz/boards/"+name, Get, kv)
        }).getOrElse(throw NecitelnaVecException)
    
    def contentOnPage(bb:BaseBot, page:String, kv:KeyValues=KeyValues()) =
        content(bb, kv++(KeyValues(("f", page))))

    def existuje:Boolean = {
        if (Klubik.neexistujeRegex.findFirstMatchIn(refContent).isEmpty) {
            true
        } else {
            false
        }
    }

    

    private def klubikInfo(byWhom:BaseBot) = byWhom.search(name).find{_.klub.name==name}.get
    def oldestNewPage(byWhom:BaseBot):Option[String] = klubikInfo(byWhom).nestarsiNove
    def isBooked(byWhom:BaseBot):Boolean = klubikInfo(byWhom).isBooked

    def canRead(who:BaseBot):Boolean = Klubik.readRegex.findFirstMatchIn(content(who)).isEmpty

    def canWrite(who:BaseBot):Boolean = Klubik.writeRegex.findFirstMatchIn(content(who)).isDefined

    
    

    //precte vsechny nove prispevky, pozpatku
    //posledni page = je dobre rict, odkud zacit, ale pokud neni zadano, najde to pres hledani
    def vsechnyNove(readByWhom:BaseBot, posledniPage: Option[String]):Iterable[Prispevek] = {
        println("chci zjistit vsechnyNove, posledniPage je "+posledniPage)
        val skutecnePosledniPage = if (posledniPage==None) {
            val nn = oldestNewPage(readByWhom)
            if (nn==None) {
                println("AAAAA")
                return None
            }
            nn.get
        } else {
            posledniPage.get
        }
        println("skutecne posledni page je "+skutecnePosledniPage)
        val prispevky = vsechnyPrispevky(readByWhom, Some(skutecnePosledniPage))
        println("VSECHNY prispevky jsou "+prispevky.size)
        val novyky = prispevky.filter{_.novyk}
        println("NOVYKY jsou "+novyky.size)
        novyky
    }

    def prispevkyStarsiNezDny(readByWhom:BaseBot, pocet:Int):Iterable[Prispevek]= {
        val starsi = new DateTime().withZone(DateTimeZone.forID("Europe/Prague")).minusDays(1)
        vsechnyPrispevky(readByWhom).takeWhile{p=>
            p.datum.isAfter(starsi)
        }
    }


    private def vsechnyPrispevkyNaStrance(readByWhom:BaseBot, page:String, backwards:Boolean): VsechnyPrispevkyNaStrance = {
        val c = contentOnPage(readByWhom, page)
        
        //iterator
        val prispevky = Klubik.articleRegex.findAllIn(c).matchData.map {data=>
           val articleId = data.group(1).toInt
           val articleMess = data.group(2)
           val replyID:Option[Int] = if (data.group(3)==null) {None} else {Some(data.group(4).toInt)}
           Prispevek(articleId, articleMess, replyID, this)
        }
        
        //timhle se mi vytrati v pripade backwards vyhoda line mapy
        //ale s tim nejde nic delat, protoze findAllIn neumi jit pozpatku
        //(mozna by slo pouzit view, ale tomu nerozumim)
        val moznaOtocene = if (backwards) {prispevky.toSeq.reverseIterator} else {prispevky}
        
        val regex = if (backwards) {Klubik.prevPageRegex} else {Klubik.nextPageRegex}
        
        val nextPageMatch = regex.findFirstMatchIn(c)
        val nextPage:Option[String] = if (nextPageMatch==None) {
            None
        } else {
            Some(nextPageMatch.get.group(1))
        }
                
        return new VsechnyPrispevkyNaStrance(moznaOtocene, nextPage)
    } 

    def vsechnyPrispevky(readByWhom:BaseBot, backwardsPage:Option[String]=None) :Iterable[Prispevek]= {
    
        val backwards: Boolean = backwardsPage.isDefined
        val beginPage = if (backwardsPage.isDefined) {backwardsPage.get} else {""}
        
        //trida, kde simuluju iterator pres klub
        //iterator proto, abych nemusel nacitat stranku, dokud se k ni nedoiteruju,
        //plus diky lazy mape ani nerozkladam regexpy prispevky dokud nemusim
        
        //k tomu si ale musim pamatovat uz receny ID (aby se tam jeden neopakoval dvakrat, protoze na konci/zacatku se obcas neco opakuje)
        //+ potrebuju specialne vyresit bug s posledni stranou s id=0
        class VsechnyPrispevkyIterator(var stav:VsechnyPrispevkyNaStrance) extends Iterator[Prispevek] {
            override def hasNext:Boolean = stav.prispevky.hasNext || stav.nextPage.isDefined || (!backwards && mameLastPagePrispevky)
            
            var usedIDs:Set[Int]=Set[Int]()
            var lastPagePrispevky:Option[Iterator[Prispevek]]=None
            
            //tato procedura PRECTE prispevky na posledni strane
            //(je to takhle, protoze nikdy nemuzu rict, jestli necekane prispevky na posledni strane fakt budou)
            def nactiLastPagePrispevky() = {
                //none==jeste jsem necetl
                if (lastPagePrispevky==None) {
                    val prispevky = vsechnyPrispevkyNaStrance(readByWhom, "0", false).prispevky.toIterable
                    val filtered = prispevky.filterNot{p=>usedIDs.contains(p.id)}
                    lastPagePrispevky = Some(filtered.iterator)
                }
            }
            def mameLastPagePrispevky():Boolean = {
                nactiLastPagePrispevky
                lastPagePrispevky.get.hasNext
            }
            def dejNextLastPagePrispevek():Prispevek = {
                nactiLastPagePrispevky
                lastPagePrispevky.get.next
            }
            
            @tailrec
            final override def next():Prispevek = {
                if (stav.prispevky.hasNext) {
                    val res = stav.prispevky.next
                    if (usedIDs.contains(res.id)) {
                        next()
                    } else {
                        usedIDs ++= Set[Int](res.id)
                        res
                    }
                } else {
                    if (stav.nextPage.isDefined) {
                        stav = vsechnyPrispevkyNaStrance(readByWhom, stav.nextPage.get, backwards)
                        next()
                    } else {
                        //nemame dalsi stranku => problem => posledni stranka ale nemusi resit opakovane ID
                        
                        //pokud backwards==1, sem bych se nemel nikdy dostat, protoze pri nedefinovanem nextpage a backwars==1 je hasnext==0
                        dejNextLastPagePrispevek
                    }
                }
            }
        }
    
        val v = vsechnyPrispevkyNaStrance(readByWhom, beginPage, backwards)
        return new VsechnyPrispevkyIterator(v).toIterable

    }
}

 

object Klubik {

    val bookTukanRegex = """(?s)<form name="markFavouriteBoardsForm".*?<input type="hidden" name="tukan" value="([^"]*)">""".r
    
    val neexistujeRegex = """<title>Okoun - neexistující klub""".r
    
    //nějaký bot je potřeba pro to, aby se načítaly informace o klubících bez odnovování
    val basebot:BaseBot = BaseBot()

    val nazevRegex = """<h2><a href="/boards/([^"]*)">""".r
    
    val favouritesRegex = """<input type="submit" value="Za""".r

    val idRegex = """boardId=([0-9]*)""".r

    val prettyRegex = """<title>klub (.*?)  na Okounovi""".r

    val writeRegex = """<span class="ico"></span>Přispět</a>""".r
    val readRegex = """(?s)<div class="listing">.*?Nemáte právo číst tento klub.*?</div>""".r
    
    val articleRegex = """(?s)<div id="article-([0-9]*)".?[^c]*class="item (.*?)(<span>[^R]*Reakce na [^<]*<a class="prev" href="/boards/[^\?]*\?contextId=([0-9]*)#.*?)?</span>[^<]*</div>[^<]*</div>""".r


    val prevPageRegex = """(?s)<li title="[^"]*"><a href="[^\?]*\?[^f]*f=([^"]*)">[^<]*</a></li>[^<]*<li class="curr"""".r
    
    val nextPageRegex = """(?s)<li class="curr" title="[^"]*">[^<]*</li>[^<]*<li title="[^"]*"><a href="[^\?]*\?[^f]*f=([^"]*)">""".r
    
    
    private def easyReg(r:scala.util.matching.Regex, s:String):String = {
        r.findFirstMatchIn(s).get.group(1)
    }

    def apply(id:Int)= {
        new Klubik(Some(id), None)
    }

    def apply(name:String)= {
         new Klubik(None, Some(name))
    }
    def apply(id:Int, name:String) = {
        new Klubik(Some(id), Some(name))
    }
    def apply(id:Int, name:String, pretty:String) = {
        new Klubik(Some(id), Some(name), Some(pretty))
    }
}

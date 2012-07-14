package cz.okoun.okobot.gaestuff
import cz.okoun.okobot.gaestuff._
import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.api.datastore.Entity
import cz.okoun.okobot.Prispevek
import org.joda.time._

import cz.okoun.okobot.Klubik
import com.google.appengine.api.datastore.FetchOptions

import com.google.appengine.api.datastore.Text
import com.google.appengine.api.datastore.Query.SortDirection
import com.google.appengine.api.datastore.Query
import com.google.appengine.api.datastore.Query.FilterPredicate;


class GAEPrispevek(p:Prispevek) {
    
    
    def saveToGAE() = {
        
        val klubikId = p.klub.id
        val klubikKey = KeyFactory.createKey("Klubik", klubikId)
        
        
        val entity = new Entity("Prispevek", p.id, klubikKey)
        
        entity.setProperty("identita", p.identita.jmeno)
        entity.setProperty("ikonkaNum", p.identita.ikonka._1)
        entity.setProperty("ikonkaStr", p.identita.ikonka._2)
        entity.setProperty("titulek", p.titulek.getOrElse(""))
        
        entity.setProperty("telo", GAEPrispevek.longText(p.telo))
        entity.setProperty("datum", p.datum.toDate)
        entity.setProperty("ancestor", p.ancestorID.getOrElse(0))
        
        datastore.put(entity)
    }
    
}

object GAEPrispevek {
    
    def loadAll:Iterable[Prispevek] = {
        import scala.collection.JavaConversions._
        
        val q = new  Query("Prispevek")
        q.addSort("datum", SortDirection.DESCENDING)
        val pq = datastore.prepare(q)
        
        val entities:Iterator[Entity] = pq.asIterator
        entities.map{e=>loadFromGAE(e)}.toIterable
    }
    
    def loadStareNDni(dny:Int):Iterable[Prispevek] = {
        import scala.collection.JavaConversions._
        
        val q = new  Query("Prispevek")
        
        val dt = new DateTime().withZone(DateTimeZone.forID("Europe/Prague")).minusDays(dny)
        
        q.setFilter(new FilterPredicate("datum",
             Query.FilterOperator.GREATER_THAN,
             dt.toDate));
        
        val pq = datastore.prepare(q)
        
        val entities:Iterator[Entity] = pq.asIterator( FetchOptions.Builder.withChunkSize(1000))
        entities.map{e=>loadFromGAE(e)}.toIterable
    }
    
    
    //can be either string or google text
    def longText(s:String) = {
        if (s.length > 300) {
            new Text(s)
        } else {
            s
        }
    }
    
    def loadFromGAE(entity:Entity):Prispevek = {
        def get(what:String):String = javaObjectToString(entity.getProperty(what))
        
        val id = entity.getKey.getId.toInt
        val jmeno = get("identita")
        val ikonkaNum = javaObjectToInt(entity.getProperty("ikonkaNum"))
        val ikonkaStr = get("ikonkaStr")
        val telo = get("telo")
        val titulek = get("titulek")
        
        val datum = javaObjectToDateTime(entity.getProperty("datum"))
        val klub = GAEKlubik.loadFromGAE(entity.getParent).getOrElse{
            val kl = Klubik(entity.getParent.getId.toInt)
            kl.saveToGAE
            GAEKlubik.loadFromGAE(entity.getParent).get
        }
        val ancestor = javaObjectToInt(entity.getProperty("ancestor"))
        
        Prispevek(klub,id,jmeno,ikonkaNum,ikonkaStr,titulek,telo,datum,ancestor)
    }
    
    def loadFromGAE(id:Int):Option[Prispevek] = {
        val key = KeyFactory.createKey("Prispevek", id)
        val entity = try {
            datastore.get(key)
        } catch {
            case e: com.google.appengine.api.datastore.EntityNotFoundException => return None
        }
        
        return Some(loadFromGAE(entity))
    }
    
}
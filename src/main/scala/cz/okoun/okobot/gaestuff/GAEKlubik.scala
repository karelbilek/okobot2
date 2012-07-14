package cz.okoun.okobot.gaestuff
import cz.okoun.okobot.gaestuff._
import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.api.datastore.Key
import com.google.appengine.api.datastore.Entity
import cz.okoun.okobot.Klubik


class GAEKlubik(klubik:Klubik) {
    def saveToGAE() = {
        if (!isInGAE) {
            val entity = new Entity("Klubik", klubik.id)
            entity.setProperty("name", klubik.name)
            entity.setProperty("prettyName", klubik.prettyName)
            
            datastore.put(entity)
        }
    }
    
    def isInGAE():Boolean = {
        val key = KeyFactory.createKey("Klubik", klubik.id)
        try {
            datastore.get(key)
        } catch {
            case e: com.google.appengine.api.datastore.EntityNotFoundException => return false
        }
        return true
    }    
}

object GAEKlubik {
    
    def loadFromGAE(key: Key):Option[Klubik] = {
        val entity = try {
            datastore.get(key)
        } catch {
            case e: com.google.appengine.api.datastore.EntityNotFoundException => return None
        }
        val id = key.getId.toInt
        val name = javaObjectToString(entity.getProperty("name"))
        val pretty = javaObjectToString(entity.getProperty("prettyName"))
        
        return Some(Klubik(id, name, pretty))
    }
    
    def loadFromGAE(id: Int):Option[Klubik]= {
        val key = KeyFactory.createKey("Klubik", id)
        loadFromGAE(key)
    }
    
}


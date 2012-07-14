package cz.okoun.okobot.gaestuff
import cz.okoun.okobot.gaestuff._
import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.api.datastore.Entity
import cz.okoun.okobot.LoginInformation

class GAELoginInformation(li: LoginInformation) {
    
    def saveToGAE() = {
        val entity = new Entity("LoginInformation", li.username)
        entity.setProperty("password", li.password)
        datastore.put(entity)
    }
    
}

object GAELoginInformation {
    def loadFromGAE(username:String):Option[LoginInformation] = {
        val key = KeyFactory.createKey("LoginInformation", username)
        val entity = try {
            datastore.get(key)
        } catch {
            case e: com.google.appengine.api.datastore.EntityNotFoundException => return None
        }
        val password = entity.getProperty("password")
        
        return Some(LoginInformation(username, javaObjectToString(password)))
        
    }
}

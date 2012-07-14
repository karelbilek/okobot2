package cz.okoun.okobot.gaestuff
import cz.okoun.okobot.gaestuff._
import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.api.datastore.Entity
import cz.okoun.okobot.LoginInformation
import com.google.appengine.api.datastore.Text

object GAECompressed {
    
    def saveToGAE(i:Int, html:String) = {
        val entity = new Entity("Compressed", i.toString)
        entity.setProperty("HTML", new Text(html))
        datastore.put(entity)
    }
    
    def loadFromGAE(i:Int):String = {
        val key = KeyFactory.createKey("Compressed", i.toString)
        val entity = datastore.get(key)
        
        
        val text = javaObjectToString(entity.getProperty("HTML"))
        
        return text
        
    }
}

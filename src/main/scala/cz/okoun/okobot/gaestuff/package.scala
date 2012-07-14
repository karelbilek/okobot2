package cz.okoun.okobot

import com.google.appengine.api.datastore.DatastoreServiceFactory
import org.joda.time.DateTime
import com.google.appengine.api.datastore.Text
import java.util.Date


             //gay stuff olol
package object gaestuff {
    val datastore = DatastoreServiceFactory.getDatastoreService
    
    implicit def klubik2GAE(k: Klubik) = new GAEKlubik(k)
    implicit def loginInfo2GAE(li: LoginInformation) = new GAELoginInformation(li)
    implicit def prispevek2GAE(p: Prispevek) = new GAEPrispevek(p)
    
    def javaObjectToString(c:AnyRef):String = {
        c match {
            case s:String => s
            case t:Text => Option(t.getValue).getOrElse("") //can be null they say
            case _ => throw new ClassCastException
        }
    }
    
    def javaObjectToInt(c:AnyRef):Int = {
        c match {
            case s:Integer => s.intValue
            case s:java.lang.Long => s.longValue.toInt
            
            case _ => throw new ClassCastException(c.getClass.getName)
        }
    }
    
    def javaObjectToDateTime(c:AnyRef):DateTime = {
        c match {
            case d:Date => new DateTime(d)
            case l:java.lang.Long => new DateTime(new Date(l))
            case _ => throw new ClassCastException(c.getClass.getName)
        }
    }

}
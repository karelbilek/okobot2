package cz.okoun.okobot



sealed abstract class HTTPType
case object Get extends HTTPType
case object Post extends HTTPType


//abych se neposral => zjednodusene HTTP requesty
object SimplifiedHTTP {
   
    //from org.apache.(something).URLEncodedUtils
    //zakoduje stringy do URL kodovani
    private def urlencode(content:String):String =  {
         val charset = java.nio.charset.Charset.forName("UTF-8")
         val safechars = new java.util.BitSet(256)
         val blankAsPlus = true
         if (content == null) {
              return null;
         }
         val RADIX = 16
         val buf:StringBuilder  = new StringBuilder();
         val bb: java.nio.ByteBuffer  = charset.encode(content);
         while (bb.hasRemaining()) {
               val b:Int = bb.get() & 0xff;
               if (safechars.get(b)) {
                   buf.append( b.toChar);
               } else if (blankAsPlus && b == ' ') {
                  buf.append('+');
               } else {
                  buf.append("%");
                  val hex1: Char = Character.toUpperCase(Character.forDigit((b >> 4) & 0xF, RADIX));
                  val hex2: Char = Character.toUpperCase(Character.forDigit(b & 0xF, RADIX));
                  buf.append(hex1);
                  buf.append(hex2);
              }
         }
         return buf.toString();
    }

    
    private def urlencodeKV(keyvalues:KeyValues):Option[String] = {
        keyvalues.map {
            case (key, value) =>
                urlencode(key)+"="+urlencode(value)        
        }.reduceLeftOption{_+"&"+_}
    }
    
    private def getUrl(url:String, keyvalues:KeyValues) = {
         val optposturl = urlencodeKV(keyvalues)
         val posturl = if (optposturl==None) {""} else {"?"+optposturl.get}
         url+posturl
    }

    
    import org.apache.http.impl.client._
    import org.apache.http._
    import org.apache.http.client.methods._
    import org.apache.http.client._
    import org.apache.http.message._
    import org.apache.http.client.entity._
    
    def read(url:String,
     httptype:HTTPType=Get, 
     keyvalues:KeyValues = KeyValues(),
     client:HttpClient):Option[String] = {
         (1 to 10).foreach{i=>
             val r = readMaybe(url, httptype, keyvalues, client)
             if (r!=None) {return r}
             Thread.sleep(i*1000)
         }
         None
     }
    
    def readMaybe(url:String,
     httptype:HTTPType=Get, 
     keyvalues:KeyValues = KeyValues(),
     client:HttpClient):Option[String] = {
         
        val method = httptype match {
            case Get => new HttpGet(getUrl(url, keyvalues))
            case Post =>
                val p = new HttpPost(url)
                val nvps = new java.util.ArrayList[NameValuePair]();
                keyvalues.foreach {
                    case (key, value) =>
                        nvps.add(new BasicNameValuePair(key,value))
                }
                p.setEntity(new UrlEncodedFormEntity(nvps));
                p
        }
        
        
        val r = try {
            Some(client.execute(method, new BasicResponseHandler()))
            
        } catch {
            case _ => None
        }
        r
    }
    
    
    
   
}

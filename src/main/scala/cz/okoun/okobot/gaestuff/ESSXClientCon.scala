//This is using (scalified) code from ESXX software package - http://esxx.org/
//(c) Martin Blom
package cz.okoun.okobot.gaestuff

import java.io._
import java.net._
import java.util.concurrent.TimeUnit
import org.apache.http._
import org.apache.http.conn._
import org.apache.http.conn.routing.HttpRoute
import org.apache.http.entity.ByteArrayEntity
import org.apache.http.message.BasicHttpResponse
import org.apache.http.params._
import org.apache.http.protocol._

import com.google.appengine.api.urlfetch._

object ESSClientCon {
    val urlFS = URLFetchServiceFactory.getURLFetchService()
}

class ESSClientCon(cm:ClientConnectionManager, var route:HttpRoute, var state:AnyRef, var closed:Boolean=true)
  extends ManagedClientConnection {
      
      
    
      var reusable:Boolean=false

      var request:HTTPRequest = null
      var response:HTTPResponse =null

      

  // From interface ManagedClientConnection

  override def isSecure() = {
    route.isSecure();
  }

  override def getRoute() = {
    route;
  }

  override def  getSSLSession():javax.net.ssl.SSLSession= {
    return null;
  }

  override def open( route:HttpRoute,  context:HttpContext,  params:HttpParams) {
    close();
    this.route = route;
//     System.err.println(">>>>");
  }

  override def tunnelTarget(secure:Boolean,  params:HttpParams) {
    throw new IOException("tunnelTarget() not supported");
  }

  override def tunnelProxy( next:HttpHost,  secure:Boolean,  params:HttpParams) {
    throw new IOException("tunnelProxy() not supported");
  }

  override def layerProtocol( context:HttpContext,  params:HttpParams) {
    throw new IOException("layerProtocol() not supported");
  }

  override def markReusable() {
    reusable = true;
  }

  override def unmarkReusable() {
    reusable = false;
  }

  override def isMarkedReusable() = {
    reusable
  }

  override def setState(state:AnyRef) {
    this.state = state;
  }

  override def getState():AnyRef =  {
    return state;
  }

  override def setIdleDuration( duration:Long,  unit:TimeUnit) {
    // Do nothing
  }


  // From interface HttpClientConnection

  override def isResponseAvailable( timeout:Int):Boolean = {
    return response != null;
  }


  override def sendRequestHeader( request:HttpRequest) {
    try {
        
        
      val host = route.getTargetHost();

      val uri = new URI(host.getSchemeName()
			+ "://"
			+ host.getHostName()
			+ (if (host.getPort() == -1)  "" else (":" + host.getPort()))
			+ request.getRequestLine().getUri());

      this.request = new HTTPRequest(uri.toURL(),
				     HTTPMethod.valueOf(request.getRequestLine().getMethod()),
				     FetchOptions.Builder.disallowTruncate().doNotFollowRedirects());
    } catch {
        case ex:URISyntaxException => throw new IOException("Malformed request URI: " + ex.getMessage(), ex)
        case ex:IllegalArgumentException => throw new IOException("Unsupported HTTP method: " + ex.getMessage(), ex);
        
    }
    import scala.collection.JavaConversions._
    
    
    request.getAllHeaders().toArray.foreach {
        h=>this.request.addHeader(new HTTPHeader(h.getName(), h.getValue()))
    }

  }


  override def sendRequestEntity( request:HttpEntityEnclosingRequest) {
    val baos:ByteArrayOutputStream = new ByteArrayOutputStream();
    if (request.getEntity() != null) {
      request.getEntity().writeTo(baos);
    }
    this.request.setPayload(baos.toByteArray());
  }


  override def  receiveResponseHeader():HttpResponse = {
    if (response == null) {
      flush();
    }

    val nResponse:HttpResponse = new BasicHttpResponse(new ProtocolVersion("HTTP", 1, 1),
						  this.response.getResponseCode(),
						  null);

    this.response.getHeaders().toArray.foreach {
        case h:HTTPHeader=>nResponse.addHeader(h.getName(), h.getValue())
        case _ => throw new ClassCastException("UAA")
    }
    

    return nResponse;
  }


  override def receiveResponseEntity( response:HttpResponse) {
    if (this.response == null) {
      throw new IOException("receiveResponseEntity() called on closed connection");
    }

    val bae:ByteArrayEntity = new ByteArrayEntity(this.response.getContent());
    bae.setContentType(response.getFirstHeader("Content-Type"));
    response.setEntity(bae);

    //???nechapu
    //response = null;
  }

  override def flush() {
    if (request != null) {
      try {
          response = ESSClientCon.urlFS.fetch(request);
          request = null;
      }catch  {
          case ex:IOException=>
            ex.printStackTrace();
            throw ex;
      }
    }
    else {
      response = null;
    }
  }


  // From interface HttpConnection

  override def close() {
    request  = null;
    response = null;
    closed   = true;
  }

  override def isOpen():Boolean = {
    request != null || response != null;
  }

  override def isStale():Boolean = {
     !isOpen() && !closed;
  }

  override def setSocketTimeout( timeout:Int) {
  }

  override def getSocketTimeout():Int= {
    return -1;
  }

  override def shutdown() {
    close();
  }

  override def  getMetrics():HttpConnectionMetrics ={
    return null;
  }


  // From interface HttpInetConnection

  override def  getLocalAddress():InetAddress= {
    return null;
  }

  override def getLocalPort():Int ={
    return 0;
  }

  override def getRemoteAddress():InetAddress ={
    return null;
  }

  override def  getRemotePort():Int=  {
    val host:HttpHost = route.getTargetHost();
    return cm.getSchemeRegistry().getScheme(host).resolvePort(host.getPort());
  }


  // From interface ConnectionReleaseTrigger

  override def releaseConnection()  {
    cm.releaseConnection(this, java.lang.Long.MAX_VALUE, TimeUnit.MILLISECONDS);
  }

  override def abortConnection() {
    unmarkReusable();
    shutdown();
  }

}
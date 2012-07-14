//This is using (scalified) code from ESXX software package - http://esxx.org/
//(c) Martin Blom
package cz.okoun.okobot.gaestuff

import java.net._;
import java.util.concurrent.TimeUnit;
import org.apache.http.conn._;
import org.apache.http.params._;
import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.conn.scheme._;

class ESSXConManager extends ClientConnectionManager {
  
  
    val no_socket_factory = new SocketFactory() {
	    def connectSocket(sock:Socket,host:String,port:Int, 
				     localAddress:InetAddress, localPort:Int, 
				     params:HttpParams):Socket = {
	            null
	    }

	    def createSocket():Socket= {
	        null
	    }

	    def isSecure(s:Socket):Boolean= {
	        false
	    }
    };

    val schemeRegistry = new SchemeRegistry();
    schemeRegistry.register(new Scheme("http",  no_socket_factory, 80));
    schemeRegistry.register(new Scheme("https", no_socket_factory, 443));

    private def getConnection( route:HttpRoute, state:AnyRef):ManagedClientConnection = {
        return new ESSClientCon(this, route, state);
    }

  override def getSchemeRegistry():SchemeRegistry = {
    schemeRegistry;
  }

  override def requestConnection( route:HttpRoute, 
							      state:AnyRef):ClientConnectionRequest = {
    return new ClientConnectionRequest() {
      def abortRequest() = {
      }

      def  getConnection(timeout:Long,tunit:TimeUnit):ManagedClientConnection = {
	      ESSXConManager.this.getConnection(route, state);
      }
    };
  }

  override def releaseConnection(conn:ManagedClientConnection, 
					  validDuration:Long, timeUnit:TimeUnit) {
  }

  override def closeIdleConnections(idletime:Long, tunit:TimeUnit) {
  }

  override def closeExpiredConnections() {
  }

  override def shutdown() {
  }

}
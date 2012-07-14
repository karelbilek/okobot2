package cz.okoun.okobot.gaestuff.servlets

import cz.okoun.okobot._
import cz.okoun.okobot.gaestuff._
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse


import cz.okoun.okobot.importantFinder._
import cz.okoun.okobot.importantFinder.ImportantFinder._

    //super jmeno :)
class ImportantServlet extends HttpServlet {
    
    
    
    override def doGet(req: HttpServletRequest, resp: HttpServletResponse)  {
        
        val dni:Int = Option(req.getParameter("dni")).getOrElse("") match {
            case ""=>1
            case w => try {
                if (w.toInt>0) w.toInt else 1
            } catch {
                case _ => 1
            }
        }
        
        resp.setContentType("text/html; charset=utf-8")
        resp.setCharacterEncoding("UTF-8")
        def myprintln(x:Any) = resp.getWriter.println(x)
        myprintln(GAECompressed.loadFromGAE(dni))
    }
    
}
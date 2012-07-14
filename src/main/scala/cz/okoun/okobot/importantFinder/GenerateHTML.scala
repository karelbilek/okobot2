package cz.okoun.okobot.importantFinder
import cz.okoun.okobot.importantFinder._
import cz.okoun.okobot._
import cz.okoun.okobot.gaestuff._
import org.joda.time.DateTime
import org.joda.time.DateTimeZone

//tohle je hnusny, ale nechce se mi moc resit, co jinak s tim :/
//sorry, trochu se za to stydim
//jestli me neco nebavi, tak je to hrani si s HTML, JavaScriptem a CSS, tak jsem to zkopnul z hnusneho perl kodu
object GenerateHTML {
    
    lazy val konec:StringBuilder = new StringBuilder("""</body></html>""")
    
    def zacatek(dni:Int):StringBuilder = {
        val r = new StringBuilder
        r ++= """<html>
        <head>
        <style type="text/css">
        .ctable{
            padding-bottom: 0px;
            border-color: #E9E9E9;
            border-width: 1px 0px 0px 0px;
            border-style: solid;
            width:100%

        }
        .icontd {
            vertical-align:top;
            width:40px;
        }

        .cislotd {
            vertical-align: top;
            width: 1.2em;
            color: gray;
            font-size: 2em;
            font-family: Helvetica, Arial;
        }
        body {
            font-family: Verdana,"Bitstream Vera Sans",Arial,sans-serif;
            margin-left:0px;
        }

        .texttdout {
            text-align:left;
            vertical-align:top;
            padding-right:100px;;
        }

        .reply {
            color:gray;
            font-size:80%;
        }

        .okodkaz, .info {
            font-size:80%;
        }

        .okodkaz {
            color:gray;
        }
        </style>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
            <title>okoun autocompressor</title>
        </head>
        <body>
        <h3>okoun autocompressor</h3>
        dny = """++dni.toString++""" | [ možnosti:
        <a href="autocompressor?dni=1">1 </a> -
        <a href="autocompressor?dni=2">2 </a> -
        <a href="autocompressor?dni=3">3 </a> -
        <a href="autocompressor?dni=4">4 </a> -
        <a href="autocompressor?dni=5">5 </a> -
        <a href="autocompressor?dni=6">6 </a> -
        <a href="autocompressor?dni=7">7 </a>
        ] | aktualizováno v 10:00, 18:00, 2:00 | poslední aktualizace: """++new DateTime().withZone(DateTimeZone.forID("Europe/Prague")).toString("dd. MM. yy HH:mm:ss")++"""
        <div style="position:absolute;top:0;right:0"><a href="http://www.toplist.cz/"><script language="JavaScript" type="text/javascript">
        <!--
        document.write('<img src="http://toplist.cz/count.asp?id=1159168&logo=mc&http='+escape(document.referrer)+'&t='+escape(document.title)+
        '&wi='+escape(window.screen.width)+'&he='+escape(window.screen.height)+'&cd='+escape(window.screen.colorDepth)+'" width="88" height="60" border=0 alt="TOPlist" />'); 
        //--></script><noscript><img src="http://toplist.cz/count.asp?id=1159168&logo=mc" border="0"
        alt="TOPlist" width="88" height="60" /></noscript></a></div>
        <script language="javascript">
        function zobraz(id) {
            var ele = document.getElementById("bd"+id);
            var text = document.getElementById("sw"+id);
            if(ele.style.display == "block") {
                        ele.style.display = "none";
                    text.innerHTML = "zobraz";
                  }
                else {
                    ele.style.display = "block";
                    text.innerHTML = "skryj";
                }
        }
        </script>"""
        r
    }
        
    def vygeneruj(dni:Int, ktisku:Seq[ClusterKTisku]):String = {
        val res = new StringBuilder
        
        
        
        res ++= zacatek(dni)
        ktisku.zipWithIndex.foreach {case (klastr,i)=>
            res ++= clusterToString(klastr,i+1)
        }
        
        res ++=konec;
        res.toString
    }
    
    def ikonkaToString(identita:Identita):StringBuilder = {
        new StringBuilder( "http://www.okoun.cz/ico/?i="+ identita.ikonka._1+"&l="+ identita.ikonka._2)
    }
    
    def clusterToString(cluster: ClusterKTisku, i:Int):StringBuilder = {
        val prispevek = cluster.prispevky(0).prispevek
        
        var res = new StringBuilder
        res ++= """<table class="ctable"><tr><td class="cislotd">"""++i.toString++"""</td><td class="icontd"><img src=""""++ikonkaToString(prispevek.identita)++
            """" width="40" height="50"></td><td class="texttdout">"""
    
        res ++= "<b>"++ prispevek.klub.prettyName++"""</b> <span class="info">"""+
                prispevek.datum.toString("dd. MM. yy HH:mm:ss") ++ 
                """</span> <span class="info">velikost vlákna """ ++
                cluster.velikost.toString ++
                "</span>"
        
        res ++= """<br><a href="javascript:zobraz("""++prispevek.id.toString++""");" id="sw"""++
                prispevek.id.toString++""""> zobraz</a><br>"""
        
        res ++= """<div id="bd"""++prispevek.id.toString++""""style ="display:none">""";
        
        cluster.prispevky.foreach {
            p=>res++=prispevekToString(p)
        }
        
        res ++= "</div>";
        res ++= "</td></tr></table>"
        
        res
    }
    
    def nedulezityToString(p:Prispevek):StringBuilder = {
        var res = new StringBuilder("""<div class="reply">""")
        res ++= "<b>" ++ p.identita.jmeno ++"</b>"
        res ++= """ - <a href="javascript:zobraz("""++
                p.id.toString++
                """);" id="sw"""++
                p.id.toString++"""">zobraz</a><br>"""
        res ++= """<div id="bd"""++p.id.toString++"""" class="replyin" style="display:none">""";
        res ++= p.telo
        res ++= "<br><br></div></div>"
        res
    }
    
    def prispevekToString(p: PrispevekKTisku):StringBuilder = {
        val prispevek = p.prispevek
        
        var res = new StringBuilder
        res ++= """<table class="ctable"><tr><td class="icontd"><img src=""""++ikonkaToString(prispevek.identita)++"""" width="40" height="50"></td><td class="texttdin">"""
        
        res ++= """<b>""" ++ prispevek.identita.jmeno ++ """</b> <a href="http://www.okoun.cz/boards/"""++prispevek.klub.name++"?contextId="++prispevek.id.toString++"#article-"++prispevek.id.toString++"""" class="okodkaz">kontext</a> <br>"""
        
        res ++= "<br>"
        prispevek.titulek.foreach {
            t=>
            res ++= "<b>"++t++"</b><br>"
        }
        res ++= prispevek.telo
        res ++= "<br><br>"
        
        
        p.duleziteDeti.foreach(
            d=>res++=prispevekToString(d)
        )
        
        p.neduleziteDeti.foreach(
            nd=> res++=nedulezityToString(nd)
        )
        res ++= "</td></tr></table>";
        res
    }
    
}
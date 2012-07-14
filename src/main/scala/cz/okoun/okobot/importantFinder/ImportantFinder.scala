package cz.okoun.okobot.importantFinder

import cz.okoun.okobot._


case class PrispevekInformace(
    prispevek:Prispevek,
    score:Float,
    level:Int,
    nejvyssiPredek:Prispevek,
    deti:Seq[Prispevek]
)

case class PrispevekKTisku (
    prispevek:Prispevek,
    duleziteDeti:Seq[PrispevekKTisku],
    neduleziteDeti:Seq[Prispevek]
)

case class ClusterKTisku (
    prispevky:Seq[PrispevekKTisku],
    nejvyssiHodnota: Float,
    nejnovejsiPrispevek:Prispevek,
    velikost:Int
)

object ImportantFinder {
    
    def redukujNaStromy(prispevkyInfo: Seq[PrispevekInformace]):ClusterKTisku = {
        val nejnovejsiPrispevek = prispevkyInfo.sortBy{_.prispevek.id}.last
        val nejvyssiHodnota = prispevkyInfo.sortBy{_.score}.last.score
        
        val map:Map[Prispevek, PrispevekInformace] = prispevkyInfo.map{p=>(p.prispevek, p)}.toMap
        
        val otcove:Set[Prispevek] = map.keys.toSet
        val deti:Set[Prispevek] = prispevkyInfo.flatMap{_.deti}.toSet
        val koreny = otcove -- deti
        val korenySeq = koreny.toSeq.sortBy{p=>(map(p).level,p.id)}
        
        def stromuj(pi:PrispevekInformace):PrispevekKTisku = {
            val prispevek = pi.prispevek
            val neduleziteDeti = pi.deti.toSet -- otcove
            val duleziteDeti = pi.deti.toSet & otcove
            val duleziteDetiKTisku = duleziteDeti.map {
                dite=>stromuj(map(dite))
            }
            PrispevekKTisku(
                prispevek, 
                duleziteDetiKTisku.toSeq.sortBy{_.prispevek.id}, 
                neduleziteDeti.toSeq.sortBy{_.id})
        }
        
        val prispevky = korenySeq.map{p=>stromuj(map(p))}
        ClusterKTisku(prispevky, nejvyssiHodnota, nejnovejsiPrispevek.prispevek, prispevkyInfo.size)
    }
    
    
    def score(level:Int):Float = (1.toDouble/math.pow(2, level-1)).toFloat
    
    
    def allImportant(prispevky:Iterable[Prispevek]) :Seq[ClusterKTisku] = {
        
        println("prispevku je :"+prispevky.size)
        
        val it : Iterable[Iterable[PrispevekInformace]] = 
            prispevky.groupBy(_.klub).mapValues{py=>importantFromClub(py)}.values
        mostImportantFromAllClubs(it)
    } 
    
    def mostImportantFromAllClubs(importants:Iterable[Iterable[PrispevekInformace]]) :Seq[ClusterKTisku] = {
        val bigMap = importants.reduceLeft{_++_}.toSeq
        
        val best = bigMap.sortBy{_.score}.reverse.take(40).map{
            p=>PrispevekInformace(p.prispevek, p.score, p.level, p.nejvyssiPredek, p.deti.sortBy{_.id})
        }
                
        val clusters:Map[Prispevek, Seq[PrispevekInformace]] = best.groupBy{_.nejvyssiPredek}
        
        val clusteryKTisku:Seq[ClusterKTisku] = clusters.values.toSeq.map{p=>redukujNaStromy(p)}
        
        val serazene = clusteryKTisku.
            sortBy{cluster=>(cluster.nejvyssiHodnota, cluster.nejnovejsiPrispevek.id)}.reverse
        serazene
    }
    
    
    def importantFromClub(prispevky: Iterable[Prispevek]):Iterable[PrispevekInformace] = {
        
        val prispevkyMapa: Map[Int, Prispevek] = prispevky.map{p=>(p.id, p)}.toMap
        
            //kvuli tomu, aby nekdo, kdo vicekrat odpovi
            //na tentyz prispevek byl zapocitan jenom jednou
        var zapocitaniAutoriOdpovedi:Set[Pair[Identita, Prispevek]] = Set[Pair[Identita, Prispevek]]()
        
        println("Hezkych je :"+ prispevky.filter{
            p=> p.ancestor(prispevkyMapa).isDefined
        }.size)

        val kSouctu:Iterable[PrispevekInformace] = prispevky.filter{
            p=> p.ancestor(prispevkyMapa).isDefined
        }.flatMap { prispevek=>
            
            
            var predekHledac:Option[Prispevek] = Some(prispevek)
            val autor = prispevek.identita
            
            val prvniPredek = prispevek.ancestor(prispevkyMapa).get
            val kombinace = (autor, prvniPredek)
            
            if (!zapocitaniAutoriOdpovedi.contains(kombinace)) {
                zapocitaniAutoriOdpovedi+=kombinace
                
                val predci = Iterator.continually {
                    predekHledac = predekHledac.get.ancestor(prispevkyMapa)
                    predekHledac
                }.takeWhile {_.isDefined}.map{_.get}.toSeq.zipWithIndex
                
                val nejvyssiPredek = predci.last._1
                val levelNejvyssiho = predci.last._2
                
                predci.map {
                    case (`prvniPredek`, level) => 
                        PrispevekInformace(prvniPredek, score(level), levelNejvyssiho-level, nejvyssiPredek, List(prispevek))
                    case (predek, level) => 
                        PrispevekInformace(predek, score(level), levelNejvyssiho-level, nejvyssiPredek, List())
                }
                
            } else {
                None
            }
        }
        kSouctu.groupBy{_.prispevek}.mapValues{_.reduceLeft{
             (i1,i2)=>
                    if (i1.level!=i2.level) {
                        throw new IllegalArgumentException("Nerovna se mrcha l "+i1.level+" a "+i2.level)
                    }
                    if (i1.nejvyssiPredek!=i2.nejvyssiPredek) {
                        throw new IllegalArgumentException("Nerovna se mrcha p "+i1.nejvyssiPredek+" a "+i2.nejvyssiPredek)
                    }
                    PrispevekInformace(i1.prispevek, i1.score+i2.score, i1.level, i1.nejvyssiPredek, i1.deti++i2.deti)
            }
        }.values
    }
    
    def main(args:Array[String]) {
        import cz.okoun.okobot._
        
        val bot = BaseBot("bot_almighty", "SUPERTAJNEHESLO")
        val klubik = Klubik("komentare_k_aktualitam")
        val klubik2 = Klubik(4367)
        
        
        val p = klubik.prispevkyStarsiNezDny(bot, 7) ++ klubik2.prispevkyStarsiNezDny(bot, 7)
        
        
        
        println(p.size) //forces to download everything
        println("OK, lets count!===============")
        val i = allImportant(p)
        println(i)
    }
}

package dojo

import collection.immutable.List

object FunsWithLists {

  def labels(ls: List[Game]) = ls.map(_.label)

  def averageRatingsOf(l:String,  ls:List[Game]) = (0/:ls.filter(_.label==l))(_+_.rating)/ls.count(_.label==l)

  def totalRatingsOf(ls: List[Game]):Int = ls match {
    case Nil => 0
    case g::rls => g.rating+totalRatingsOf(rls) 
  }

  def totalRatingsOfLabel(label: String, list: List[Game]):Int = {
    var total = 0
    for(g:Game <- list){
      if(g.label == label) total +=g.rating
    }
    total
  }

  def increaseRatingBy(inc: Int, ls: List[Game]) = ls.map((g:Game) => Game(g.label,g.rating+inc))

  def decreaseRatingBy(i: Int, s: String, list: List[Game]) = list.map((g:Game) => Game(g.label,if(g.label==s)g.rating-i else g.rating))

  def createFunctionToFindGamesByLabel(label: String):(List[Game]) => List[Game] = (l:List[Game]) => l.filter(_.label == label)

  def zipWithKey = (f: (Game) => String, ls: List[Game]) => ls.map(f(_)) zip ls

}

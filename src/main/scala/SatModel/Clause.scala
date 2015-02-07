package SatModel

import scala.collection.mutable.ListBuffer

/**
 * Created by vladimir on 1/5/15.
 */
class Clause () {
  //mb in future add constructor with "terms : List[SatModel.Term], negation : List[Boolean]"
  var clauseTerms : ListBuffer[(Term, Boolean)] = ListBuffer()

  def addTerm(term: Term, negation: Boolean): Unit = {
    clauseTerms += ((term, negation))
  }

  
  def isSat(values : List[Boolean]) : Boolean = !clauseTerms.forall(t=> (values.apply(t._1.getId-1) ^ t._2) == false)
  

}
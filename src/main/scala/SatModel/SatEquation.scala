package SatModel

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

/**
 * Created by vladimir on 1/5/15.
 */
class SatEquation {
  var maxWeight = 0

  var termsCount : Int = 0

  def getTerms = {
    var terms = List[Term]()
    clauses.foreach(cl => {
      cl.clauseTerms.foreach(t=> {
        if (!terms.contains(t._1)) {
          terms :+= t._1
        }
      })
    })
    terms
  }

  def isSat(values : List[Boolean]) : Boolean = numOfSatisfiedClauses(values) == clauses.length

  var clauses : List[Clause] = List()

  def addClause(clause: Clause): Unit = clauses :+= clause

  def numOfSatisfiedClauses(values : List[Boolean]) : Int = clauses.count(_.isSat(values))

  def termsWeight(values : List[Boolean]) = values.zip(getTerms).filter(_._1 == true).map(_._2.getWeight).sum

}
package SatParser

import SatModel.{Clause, SatEquation, Term}

/**
 * Created by vladimir on 1/5/15.
 */
object DimacsParser extends SatParser {
  def parse(lines: Iterator[String]): SatEquation = {
    var terms: List[Term] = List()
    val satEquation: SatEquation = new SatEquation()
    lines.filter(_.charAt(0) match {
      case 'c' => false
      case 'p' => true
      case '0' => false
      case '%' => false
      case _ => true
    }).foreach(line => {
      val lineList = line.trim().split(' ')
      if (lineList.apply(0) == "w") {
        //weights
        lineList.drop(1).dropRight(1).zipWithIndex.map(elem => terms :+= new Term(elem._2.toInt +1, elem._1.toInt))
      }
      else if(lineList.apply(0) == "p") {
        satEquation.termsCount = lineList.drop(2).head.toInt
        satEquation.maxWeight = lineList.drop(4).head.toInt
      }
      else {
        //clauses
        val clause: Clause = new Clause
        lineList.dropRight(1).map(v => {
          if (v.charAt(0) == '-')
            clause.addTerm(terms.find(t => t.getId == v.drop(1).toInt).get, true)
          else clause.addTerm(terms.find(t => t.getId == v.toInt).get, false)
        })
        satEquation.addClause(clause)
      }
    })
    satEquation
  }
}


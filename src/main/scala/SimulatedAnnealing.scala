import SatModel.SatEquation

import scala.util.Random

/**
 * Created by vladimir on 2/3/15.
 */
class SimulatedAnnealing (satEquation: SatEquation) {
  val n = satEquation.termsCount
  val rand = new Random()

  def computeEquation (equilibrium: Int, startTemp: Double, endTemp: Double, coolingCoefficient: Double) = {
    var temp = startTemp
    var values = (1 to n).map(x=> true).toList

    var bestValues = List[Boolean]()
    var valuesFitnessWeight = fitness(values)
    var bestFitnessWeight = 0


    while (temp > endTemp) {
      for(_ <- 1 to equilibrium) {
        val newValues = getNewValues(values, bestFitnessWeight, temp);
        val newValuesFitnessWeight = fitness(newValues)

        if (newValuesFitnessWeight > valuesFitnessWeight && satEquation.isSat(newValues) ) {
          bestValues = newValues
          bestFitnessWeight = newValuesFitnessWeight
        }

        values = newValues
        valuesFitnessWeight = newValuesFitnessWeight
      }

      temp = temp * coolingCoefficient
    }
    bestValues
  }

  private def getNewValues(values: List[Boolean], bestWeight: Int, temp: Double): List[Boolean] = {
    val randBit = rand.nextInt(n)
    val newValues = values.updated(randBit, !values(randBit))
    val newValuesWeight = fitness(newValues);
    val stateFitness = fitness(values)
    if (newValuesWeight > bestWeight) newValues
    else {
      if (rand.nextDouble() < Math.pow(Math.E, -(newValuesWeight - stateFitness) / temp)) newValues
      else values
    }
  }

  def fitness(values: List[Boolean]): Int = {
    val satisfiedClauses = satEquation.numOfSatisfiedClauses(values)
    val weight = satEquation.termsWeight(values)
    var fitnessWeight = weight  + satisfiedClauses * satEquation.maxWeight
    if (satisfiedClauses == satEquation.clauses.length) fitnessWeight += satEquation.maxWeight * satEquation.clauses.length
    fitnessWeight
  }
}

import java.io.{FileOutputStream, PrintWriter}

import SatModel.SatEquation
import SatParser.DimacsParser
import scala.util.Random

object sat_solver {
  def main(args: Array[String]) {
   //   DimacsGenerator.GenerateInstanceFiles("data", 5, 15, 3.5f, 40)
    Solve
   }

  def Solve {
    val path = "data/"
    import java.io.File
    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    val files = recursiveListFiles(new File(path)).filter(f => !f.isDirectory && f.getName.endsWith(".cnf")).sortBy(_.getName)
    val eqList = files.map(f => {
      //read file
      val source = scala.io.Source.fromFile(f)
      val lines = source.getLines()
      val equation = DimacsParser.parse(lines)
      source.close()
      equation
    })
    new SimulatedAnnealing(eqList.apply(0)).computeEquation(1, 2, 1, 0.1)
    //logger
    val pw = new PrintWriter(new FileOutputStream("results.txt", true))
    pw.println(path)

    val startTime = System.nanoTime

    val bestRepeat = 3

    var solvedFilesCount = 0
    var bestWeight = 0

    eqList.foreach(eq => {
      val sa = new SimulatedAnnealing(eq)
      //SA configuration
      val equilibrium = eq.getTerms.length * 8
      val startTemp = 7000
      val endTemp = 2
      val coolingCoefficient = 0.98

      //BestLoops Configuration
      var best = 0
      var fileWeight: Int = 0

      for (_ <- 1 to bestRepeat) {
        val result = sa.computeEquation(equilibrium, startTemp, endTemp, coolingCoefficient)

        if (result.length != 0 && eq.termsWeight(result) > 0)
          best = eq.termsWeight(result)
      }
      if (best != 0) {
        solvedFilesCount += 1
        fileWeight = best
      }
      bestWeight += fileWeight


    })

    val time = (System.nanoTime - startTime) / (1000000000d * bestRepeat)

    if (solvedFilesCount == 0)
      pw.println("solvedFilesCount = 0")
    else
      pw.println(" time: " + time + " wlist: " + bestWeight / solvedFilesCount + " solved " + solvedFilesCount)

    pw.close()
    //fsdfs
  }
}





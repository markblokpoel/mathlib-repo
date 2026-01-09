package categorization

import mathlib.set.SetTheory._

import scala.util.Random

object Categorization {

  def toDotString(
      objects: Set[String],
      sim: (String, String) => Double,
      dis: (String, String) => Double
  ): String = {
    "graph{" +
      objects.combinations2
        .map {
          case (a: String, b: String) =>
            s"$a--$b[color=orange,penwidth=${5 * sim(a, b)}];$a--$b[color=purple,penwidth=${5 * dis(a, b)}];"

        }
        .mkString("") +
      "}"
  }

  def categorization(
      objects: Set[String],
      sim: (String, String) => Double,
      dis: (String, String) => Double
  ): Set[Set[Set[String]]] = {
    def withinSim(category: Set[String]): Double =
      sum(category.combinations2, sim)

    def betweenDis(
        categoryA: Set[String],
        categoryB: Set[String]
    ): Double =
      sum(categoryA x categoryB, dis)

    def fun(partition: Set[Set[String]]): Double =
      sum(partition, withinSim _) +
        sum(partition.combinations2, betweenDis _)

    val allPartitions = objects.allPartitions
    argMax(allPartitions, fun) // Extensionally equivalent
    // .random.get // Extensionally equivalent with probabilistic
    // .head // Extensionally equivalent with single-valued code (does it work with set of categorizations)
  }

  case class Input(
      objects: Set[String],
      sim: (String, String) => Double,
      dis: (String, String) => Double
  )

  def randomInput(n: Int, mean: Double = 0.5, sdSim: Double = 1.0, sdDis: Double = 1.0): Input = {
    val objects = (0 until n).map(i => s"Object $i").toSet
    val simMap  = (objects x objects).map(pair => pair -> nextGaussian(mean, sdSim, Some(0.0), Some(1.0))).toMap
    def sim(a: String, b: String): Double = simMap.getOrElse((a, b), 0)
    val disMap = (objects x objects).map(pair => pair -> nextGaussian(mean, sdDis, Some(0.0), Some(1.0))).toMap
    def dis(a: String, b: String): Double = disMap.getOrElse((a, b), 0)
    Input(objects, sim, dis)
  }

  def nextGaussian(
      mean: Double,
      sd: Double,
      lowerbound: Option[Double] = None,
      upperbound: Option[Double] = None
  ): Double = {
    var v1 = .0
    var v2 = .0
    var s  = .0
    do {
      v1 = 2 * Random.nextDouble - 1 // between -1.0 and 1.0
      v2 = 2 * Random.nextDouble - 1 // between -1.0 and 1.0
      s = v1 * v1 + v2 * v2
    } while (s >= 1 || s == 0)
    val multiplier = StrictMath.sqrt(-2 * StrictMath.log(s) / s)
    val rg = mean + sd * v1 * multiplier
    if(lowerbound.isDefined && rg < lowerbound.get) lowerbound.get
    else if(upperbound.isDefined && rg > upperbound.get) upperbound.get
    else rg
  }

  def main(args: Array[String]): Unit = {
    val numberOfObjects = 10
    val numberOfSimulations = 10
    val sdCombinations = Set(0.05, 0.1, 0.5) x Set(0.05, 0.1, 0.5)
    val simData: Map[(Double, Double), Seq[(Input, Set[Set[Set[String]]])]] = sdCombinations.map(sdc => {
      val (sdSim, sdDis) = sdc
      val dataSeq = (0 until numberOfSimulations).map(_ => {
        val input = randomInput(numberOfObjects, mean = 0.5, sdSim, sdDis)
        val output = categorization(input.objects, input.sim, input.dis)
        (input, output)
      })
      sdc -> dataSeq
    }).toMap

    simData.view.mapValues((dataSeq: Seq[(Input, Set[Set[Set[String]]])]) => {
      val averageNumberOfSolutions = dataSeq.map(_._2.size).sum / numberOfSimulations.toDouble
      val averageNumberOfCategories = dataSeq.map(_._2.map(_.size).sum).sum / numberOfSimulations.toDouble
      (averageNumberOfSolutions, averageNumberOfCategories)
    }).foreach(println)

//    simData.foreach(combo => {
//      println(combo._1)
//      combo._2.foreach(println)
//    })

//    Categorization.categorization(input.objects, input.sim, input.dis)
//    println("sd, n")
//    (0 until 200)
//      .foreach(_ => {
//        println("0.05, " + nextGaussian(0.5, .05, Some(0.0), Some(1.0)))
//        println("0.1, " + nextGaussian(0.5, .1, Some(0.0), Some(1.0)))
//        println("0.5, " + nextGaussian(0.5, .5, Some(0.0), Some(1.0)))
//      })


  }

}

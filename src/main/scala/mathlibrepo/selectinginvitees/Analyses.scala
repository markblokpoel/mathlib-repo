package mathlibrepo.selectinginvitees

import mathlib.set.SetTheory._

object Analyses {
  def analysis1(io: (Input, Set[Person])): (Double, Double) = {
    val input      = io._1
    val output     = io._2
    val nrLikes    = input.group.uniquePairs.count(input.like.tupled)
    val nrDislikes = input.group.uniquePairs.count(!input.like.tupled(_))
    val ldRatio    = nrLikes.toDouble / nrDislikes
    val size       = output.size.doubleValue
    (ldRatio, size)
  }

  def analysis2(io: (Input, Set[Person])): (Double, Double) = {
    val input      = io._1
    val output     = io._2
    val nrLikes    = input.group.uniquePairs.count(input.like.tupled)
    val nrDislikes = input.group.uniquePairs.count(!input.like.tupled(_))
    val ldRatio    = nrLikes.toDouble / nrDislikes
    val avgLikes   = output.uniquePairs.count(input.like.tupled)
    (ldRatio, avgLikes)
  }

  def scatterDataToString(data: List[(Double, Double)], label: String = ""): String =
    s"{label:'$label',data:[" +
      data.map { case (i, o) => s"{x:$i,y:$o}" }.mkString(",") +
      "]}"
}

package mathlibrepo.selectinginvitees

import mathlib.set.SetTheory._

object SelectingInviteesTest {
  def main(args: Array[String]): Unit = {
    val groupSize = 6
    val likeDislikeRatios = Set(0, 0.22, 0.66, 1.0)
    val pairLikeRatios = Set(0, 0.22, 0.66, 1.0)
    val ks = Set(0, 0.22, 0.66, 1.0)
    val sampleSize = 1

    val inputs: List[SelectingInvitees.Input] =
      (for(likeDislikeRatio <- likeDislikeRatios;
           pairLikeRatio <- pairLikeRatios;
           k <- ks) yield {
        SelectingInvitees.inputGenerator(
          groupSize,
          likeDislikeRatio,
          pairLikeRatio,
          (k * groupSize).intValue,
          sampleSize
        )
      }).toList.flatten

    def analysis1(io: (SelectingInvitees.Input, Set[Person])): (Double, Double) = {
      val input      = io._1
      val output     = io._2
      val nrLikes    = input.group.uniquePairs.count(input.like.tupled)
      val nrDislikes = input.group.uniquePairs.count(!input.like.tupled(_))
      val ldRatio    = nrLikes.toDouble / nrDislikes
      val size       = output.size.doubleValue
      (ldRatio, size)
    }

    def analysis2(io: (SelectingInvitees.Input, Set[Person])): (Double, Double) = {
      val input = io._1
      val output = io._2
      val nrLikes = input.group.uniquePairs.count(input.like.tupled)
      val nrDislikes = input.group.uniquePairs.count(!input.like.tupled(_))
      val ldRatio = nrLikes.toDouble / nrDislikes
      val avgLikes = output.uniquePairs.count(input.like.tupled)
      (ldRatio, avgLikes)
    }

    val io4 = inputs.map(input =>
      input -> SelectingInvitees.si4(
        input.group,
        input.personsLiked,
        input.personsDisliked,
        input.like,
        input.k
      )
    )
    val io5 = inputs.map(input =>
      input -> SelectingInvitees.si5(
        input.group,
        input.personsLiked,
        input.personsDisliked,
        input.like
      )
    )
    val io6 = inputs.map(input =>
      input -> SelectingInvitees.si6(
        input.group,
        input.personsLiked,
        input.personsDisliked,
        input.like,
        input.k
      )
    )

    val data4: List[(Double, Double)] = io4 map {
      case (i: SelectingInvitees.Input, o: Set[Person]) => {
        analysis1(i, o)
      }
    }
    val data5: List[(Double, Double)] = io5 map {
      case (i: SelectingInvitees.Input, o: Set[Person]) => {
        analysis1(i, o)
      }
    }
    val data6: List[(Double, Double)] = io6 map {
      case (i: SelectingInvitees.Input, o: Set[Person]) => {
        analysis1(i, o)
      }
    }

    val data4string = "{label:'SI4',data:[" +
      data4.map { case (i, o) => s"{x:$i,y:$o}" }.mkString(",") +
      "]}"
    val data5string = "{label:'SI5',data:[" +
      data5.map { case (i, o) => s"{x:$i,y:$o}" }.mkString(",") +
      "]}"
    val data6string = "{label:'SI6',data:[" +
      data6.map { case (i, o) => s"{x:$i,y:$o}" }.mkString(",") +
      "]}"

    val quickchartURL =
      s"""
       |https://quickchart.io/chart?c={type:'scatter',data:{datasets:[$data4string,$data5string,$data6string]}}
       |""".stripMargin

    println(quickchartURL)
  }
}

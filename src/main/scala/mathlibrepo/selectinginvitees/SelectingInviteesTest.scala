package mathlibrepo.selectinginvitees

object SelectingInviteesTest {
  def main(args: Array[String]): Unit = {

    val inputs = Input.generate(
      groupSize = 6,
      likeDislikeRatios = Set(0, 0.22, 0.66, 1.0),
      pairLikeRatios = Set(0, 0.22, 0.66, 1.0),
      ks = Set(0, 0.22, 0.66, 1.0),
      sampleSize = 1
    )

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
      case (i: Input, o: Set[Person]) => {
        Analyses.analysis1(i, o)
      }
    }
    val data5: List[(Double, Double)] = io5 map {
      case (i: Input, o: Set[Person]) => {
        Analyses.analysis1(i, o)
      }
    }
    val data6: List[(Double, Double)] = io6 map {
      case (i: Input, o: Set[Person]) => {
        Analyses.analysis1(i, o)
      }
    }

    val data4string = Analyses.scatterDataToString(data4, "SI4")
    val data5string = Analyses.scatterDataToString(data5, "SI5")
    val data6string = Analyses.scatterDataToString(data6, "SI6")

    val quickchartURL =
      s"""
       |https://quickchart.io/chart?c={type:'scatter',data:{datasets:[$data4string,$data5string,$data6string]}}
       |""".stripMargin

    println(quickchartURL)
  }
}

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

    val io4 = inputs.map(input => input -> SelectingInvitees.si4(input))
    val io5 = inputs.map(input => input -> SelectingInvitees.si5(input))
    val io6 = inputs.map(input => input -> SelectingInvitees.si6(input))


    val data4A1: List[(Double, Double)] = io4 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis1(i, o)

    }
    val data5A1: List[(Double, Double)] = io5 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis1(i, o)
    }
    val data6A1: List[(Double, Double)] = io6 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis1(i, o)
    }

    val data4string1 = Analyses.scatterDataToString(data4A1, "SI4")
    val data5string1 = Analyses.scatterDataToString(data5A1, "SI5")
    val data6string1 = Analyses.scatterDataToString(data6A1, "SI6")

    val quickchartURL1 =
      s"""
       |https://quickchart.io/chart?c={type:'scatter',data:{datasets:[$data4string1,$data5string1,$data6string1]}}
       |""".stripMargin

    println(quickchartURL1)

    val data4A2: List[(Double, Double)] = io4 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis2(i, o)

    }
    val data5A2: List[(Double, Double)] = io5 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis2(i, o)
    }
    val data6A2: List[(Double, Double)] = io6 map {
      case (i: Input, o: Set[Person]) => Analyses.analysis2(i, o)
    }

    val data4string2 = Analyses.scatterDataToString(data4A2, "SI4")
    val data5string2 = Analyses.scatterDataToString(data5A2, "SI5")
    val data6string2 = Analyses.scatterDataToString(data6A2, "SI6")

    val quickchartURL2 =
      s"""
         |https://quickchart.io/chart?c={type:'scatter',data:{datasets:[$data4string2,$data5string2,$data6string2]}}
         |""".stripMargin

    println(quickchartURL2)
  }
}

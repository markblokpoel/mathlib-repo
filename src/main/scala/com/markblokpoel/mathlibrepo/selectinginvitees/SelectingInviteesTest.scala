package com.markblokpoel.mathlibrepo.selectinginvitees

import Person._

object SelectingInviteesTest {
  def main(args: Array[String]): Unit = {
    val inputs = SelectingInvitees.inputGenerator(
      groupSize = 4,
      likeDislikeRatio = 0.5,
      pairLikeRatio = 0.8,
      k = 3,
      sampleSize = 10
    )
    inputs.foreach(input => {
      val output = SelectingInvitees.si4(
        input.group,
        input.personsLiked,
        input.personsDisliked,
        input.like,
        input.k
      )
      println(input.group.toDotString(input.like))
      println(input.group.toDotString(input.personsLiked, input.personsDisliked, input.like))
      println(input)
      println(output)
    })
  }
}

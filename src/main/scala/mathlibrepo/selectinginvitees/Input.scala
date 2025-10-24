package mathlibrepo.selectinginvitees

case class Input(
    group: Set[Person],
    personsLiked: Set[Person],
    personsDisliked: Set[Person],
    like: (Person, Person) => Boolean,
    k: Int
)

object Input {
  def random(
      groupSize: Int,
      likeDislikeRatio: Double,
      pairLikeRatio: Double,
      k: Int
  ): Input = {
    val group           = Person.randomGroup(groupSize)
    val personsLiked    = group.take((groupSize * likeDislikeRatio).intValue)
    val personsDisliked = group.drop((groupSize * likeDislikeRatio).intValue)
    def like            = group.randomLikeFunction(pairLikeRatio)

    Input(group, personsLiked, personsDisliked, like, k)
  }

  def generate(
      groupSize: Int,
      likeDislikeRatios: Set[Double],
      pairLikeRatios: Set[Double],
      ks: Set[Double],
      sampleSize: Int
  ): List[Input] =
    (for (
      likeDislikeRatio <- likeDislikeRatios;
      pairLikeRatio    <- pairLikeRatios;
      k                <- ks;
      _                <- 0 until sampleSize
    ) yield {
      Input.random(
        groupSize,
        likeDislikeRatio,
        pairLikeRatio,
        (k * groupSize).intValue
      )
    }).toList
}

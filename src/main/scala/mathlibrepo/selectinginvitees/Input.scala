package mathlibrepo.selectinginvitees

case class Input(
    group: Set[Person],
    personsLiked: Set[Person],
    personsDisliked: Set[Person],
    like: (Person, Person) => Boolean,
    k: Int
)

object Input {
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
      k                <- ks
    ) yield {
      SelectingInvitees.inputGenerator(
        groupSize,
        likeDislikeRatio,
        pairLikeRatio,
        (k * groupSize).intValue,
        sampleSize
      )
    }).toList.flatten
}

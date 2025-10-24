package mathlibrepo.selectinginvitees

case class Likes(a: Person, b: Person, likes: Boolean) {
  def isAbout(pair: (Person, Person)): Boolean = {
    a == pair._1 && b == pair._2 ||
    a == pair._2 && b == pair._1
  }
  override def toString: String = if (likes) s"$a likes $b" else s"$a dislikes $b"
}

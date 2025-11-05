import mathlibrepo.selectinginvitees._
import mathlib.set.SetTheory._

/* Block 1 */
Person("Jamie")

/* Block 2 */
val person1 = Person("Jamie")
val person2 = Person("Jamie")

person1 == person2

/* Block 3 */
Person.random

/* Block 4 */
Person.randomGroup(5)

/* Block 5 */
val persons         = Person.randomGroup(5)
val personsLiked    = persons.take(2) // Take 2 people from persons.
val personsDisliked = persons \ personsLiked

println(personsLiked)
println(personsDisliked)
println(persons)

/* Block 6 */
val lela   = Person("Lela")
val carlos = Person("Carlos")
val ervin  = Person("Ervin")

println(lela likes carlos)
println(carlos dislikes ervin)
println(carlos dislikes lela)

/* Block 7 */
val lela   = Person("Lela")
val carlos = Person("Carlos")
val ervin  = Person("Ervin")

val persons        = Set(lela, carlos, ervin)
val partialLikings = Set(lela likes carlos, carlos likes ervin, carlos dislikes lela)

def likeDerived = persons.deriveLikeFunction(partialLikings)

/* Block 8 */
persons.toDotString(likeDerived)

/* Block 9 */
likeDerived(lela, carlos)
likeDerived(lela, ervin)
likeDerived(carlos, ervin)

/* Block 10 */
val lela   = Person("Lela")
val carlos = Person("Carlos")
val ervin  = Person("Ervin")

val persons = Set(lela, carlos, ervin)

def likeRandom = persons.randomLikeFunction(0.7)

persons.toDotString(likeRandom)

likeRandom(lela, carlos)
likeRandom(lela, ervin)
likeRandom(carlos, ervin)

/* Block 11 */
val persons         = Person.randomGroup(5)
val personsLiked    = persons.take(2)
val personsDisliked = persons \ personsLiked

def likeRandom2 = persons.randomLikeFunction(0.7)

persons.toDotString(personsLiked, personsDisliked, likeRandom2)

/* Block 12 */
def si4(
    persons: Set[Person],
    personsLiked: Set[Person],
    personsDisliked: Set[Person],
    like: (Person, Person) => Boolean,
    k: Int
): Set[Person] = {
  // Input must satisfy these constraints, otherwise error.
  require(personsLiked <= persons, "personsLiked must be a subset of persons")
  require(personsDisliked <= persons, "personsDisliked must be a subset of persons")
  require(
    personsLiked /\ personsDisliked == Set.empty,
    "personsLiked intersect personsDisliked must be emtpy"
  )
  require(
    personsLiked \/ personsDisliked == persons,
    "personsLiked union personsLiked must equal persons"
  )

  // Specify that invitees is valid if |G /\ D| <= k.
  def atMostKDislikes(invitees: Set[Person]): Boolean = {
    (invitees /\ personsDisliked).size <= k
  }
  // Specify the optimality condition.
  def xg(invitees: Set[Person]): Int = {
    // The number of unique pairs that like eachother.
    val x = { invitees.uniquePairs | like.tupled }.size
    // The number of total invitees.
    val g = invitees.size
    x + g
  }
  val invitees = { powerset(persons) | atMostKDislikes _ }.argMax(xg)
  // Return a (valid) set of invitees at random.
  invitees.random.get
}
/* Block 13 */

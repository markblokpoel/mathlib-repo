package mathlibrepo.selectinginvitees

import mathlib.set.SetTheory._
import scala.util.Random

case class Person(name: String) {
  override def toString: String      = name
  def likes(other: Person): Likes    = Likes(this, other, likes = true)
  def dislikes(other: Person): Likes = Likes(this, other, likes = false)
}

case object Person {
  private val names: Set[String] = Set(
    "Sora",
    "Eve",
    "Finnigan",
    "Weijun",
    "Jenessa",
    "Yong xiá",
    "Metztli",
    "Narae",
    "Ayman",
    "Harout",
    "Romane",
    "Sasha",
    "Sanghan",
    "Laurent",
    "Nayra",
    "Jae Yee",
    "Ze",
    "Delsy",
    "Taguhi",
    "Maria",
    "Young Ju",
    "Adam",
    "Lilit",
    "Mia",
    "Salwa",
    "Khachatur",
    "Jose",
    "Jiwoo",
    "Yuzhi",
    "Hisako",
    "Jae Yee",
    "Shota",
    "Finnigan",
    "Huaiyu",
    "Takuto",
    "Saba",
    "Quiztoc",
    "Jie",
    "Osamu",
    "Paola",
    "Hua",
    "Fritz",
    "Atsuya",
    "Romane",
    "Yeoreum",
    "Danbi",
    "Osamu",
    "Xochitl",
    "Greny",
    "Batool",
    "Seongmin",
    "Consolata",
    "Victoire",
    "Seulki",
    "Hai",
    "Keerat",
    "Yeoreum",
    "Luna",
    "Umeko",
    "Shuhao",
    "Kamran",
    "Anjali",
    "Boudica",
    "Armen",
    "Yui",
    "Joyce",
    "Lamar",
    "Jie",
    "Jack",
    "Shilp",
    "Jaeyun",
    "Xochitl",
    "Fritz",
    "Salwa",
    "Saki",
    "Ayman",
    "Travis",
    "Ximeng",
    "Johannes",
    "Harout",
    "Adriana",
    "Tenoch",
    "Bingyi",
    "Lixin",
    "Yang",
    "Travis",
    "Bjaka",
    "Inga",
    "Hai",
    "Armen",
    "Evan",
    "Ayaka",
    "Haneul",
    "Romane",
    "Luna",
    "Shreya",
    "Kyungmin",
    "Martijn",
    "Metztli",
    "Anne-Fatima",
    "Beverly",
    "Adriana",
    "Hai",
    "Kimberly",
    "Yerin",
    "Johannes",
    "Travis",
    "Federico",
    "Eunhye",
    "Finnigan"
  )

  def random: Person = Person(names.random.getOrElse("Easter Bunny"))

  // Returns a set of k random persons.
  def randomGroup(size: Int): Set[Person] = {
    def rg(size: Int, namesLeft: Set[String]): Set[Person] = {
      if (size == 0) Set.empty
      else {
        val newPerson = namesLeft.random
        if (newPerson.isEmpty) Set.empty
        else rg(size - 1, namesLeft - newPerson.get) + Person(newPerson.get)
      }
    }

    rg(size, names)
  }

  implicit class ImplPersons(persons: Set[Person]) {
    def deriveLikeFunction(partialLikes: Set[Likes]): (Person, Person) => Boolean = {
      val completeLike: Map[(Person, Person), Boolean] = persons.unorderedUniquePairs
        .map(pair => {
          val likeOption: Option[Likes] = partialLikes.find(_.isAbout(pair))

          if (likeOption.isDefined)
            pair -> likeOption.get.likes
          else
            pair -> false
        })
        .toMap

      def like(a: Person, b: Person): Boolean = {
        if (completeLike.contains((a, b))) completeLike((a, b))
        else false
      }

      like
    }

    def randomLikeFunction(probability: Double = 0.5): (Person, Person) => Boolean = {
      require(probability >= 0 && probability <= 1, "Probability must range from 0 and 1.")

      val completeLike: Map[(Person, Person), Boolean] = persons.unorderedUniquePairs
        .map(_ -> (Random.nextDouble <= probability))
        .toMap

      def like(a: Person, b: Person): Boolean = {
        if (completeLike.contains((a, b))) completeLike((a, b))
        else false
      }

      like
    }

    def toDotString(like: (Person, Person) => Boolean): String = {
      "graph{" +
        "node[shape=circle,width=1,fixedsize=true];" +
        persons.unorderedUniquePairs
          .map(pair => {
            if (like(pair._1, pair._2))
              s"<${pair._1}>--<${pair._2}>[style=dashed];"
            else s"<${pair._1}>--<${pair._2}>[style=solid];"
          })
          .mkString("") +
        "}"
    }

    def toDotString(
        personsLiked: Set[Person],
        personsDisliked: Set[Person],
        like: (Person, Person) => Boolean
    ): String = {"graph{" +
        "node[shape=circle,width=1,fixedsize=true,style=filled,fillcolor=darkolivegreen1];" +
      personsLiked.mkString("",",",";") +
        "node [shape=circle,width=1,fixedsize=true,style=filled,fillcolor=lightcoral];" +
      personsDisliked.mkString("", ",", ";") +
        persons.unorderedUniquePairs
          .map(pair => {
            if (like(pair._1, pair._2)) {
              s"<${pair._1}>--<${pair._2}>[style=dashed];"
            }
            else
              s"<${pair._1}>--<${pair._2}>[style=solid];"
          })
          .mkString("") +
        "}"
    }
  }
}

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
    "Nettie",
    "Lester",
    "Brian",
    "Cody",
    "Erik",
    "William",
    "Molly",
    "Joey",
    "Thelma",
    "Edgar",
    "Emanuel",
    "Sergio",
    "Herman",
    "Kelley",
    "Wilfred",
    "Guadalupe",
    "Paula",
    "Sheila",
    "Javier",
    "Kelly",
    "Jason",
    "Gilbert",
    "Harriet",
    "Meghan",
    "Kenneth",
    "Holly",
    "Rose",
    "Lela",
    "Brenda",
    "Constance",
    "Vera",
    "Ramiro",
    "Diana",
    "Charlene",
    "Betty",
    "Michelle",
    "Frederick",
    "Elmer",
    "Byron",
    "Randal",
    "Roderick",
    "Clark",
    "Mathew",
    "Sammy",
    "Colleen",
    "Marian",
    "Tyrone",
    "Keith",
    "Tonya",
    "John",
    "Kayla",
    "Johanna",
    "Dwayne",
    "Antonia",
    "Kerry",
    "Fannie",
    "Nichole",
    "Jeanne",
    "Roberto",
    "Vicky",
    "Jesus",
    "Angela",
    "Fredrick",
    "Fernando",
    "Vivian",
    "Natalie",
    "Johnnie",
    "Monica",
    "Angelica",
    "Anna",
    "Carlos",
    "Marion",
    "Henry",
    "Lawrence",
    "Alexis",
    "Garry",
    "Bernard",
    "Jana",
    "Ernestine",
    "Deborah",
    "Willard",
    "Eileen",
    "Erica",
    "Elvira",
    "Myron",
    "Elena",
    "Ervin",
    "Jeannette",
    "Veronica",
    "Abraham",
    "Lamar",
    "Wanda",
    "Lorraine",
    "Doris",
    "Leigh",
    "Devin",
    "Lindsay",
    "Isabel",
    "Marlene",
    "Betsy"
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
              s"${pair._1}--${pair._2}[style=dashed];"
            else s"${pair._1}--${pair._2}[style=solid];"
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
              s"${pair._1}--${pair._2}[style=dashed];"
            }
            else
              s"${pair._1}--${pair._2}[style=solid];"
          })
          .mkString("") +
        "}"
    }
  }
}

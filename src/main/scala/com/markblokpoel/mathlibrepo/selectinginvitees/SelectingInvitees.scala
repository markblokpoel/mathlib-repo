package com.markblokpoel.mathlibrepo.selectinginvitees

import mathlib.set.SetTheory._
import scala.util.Random

case object SelectingInvitees {
  case class Input(
      group: Set[Person],
      personsLiked: Set[Person],
      personsDisliked: Set[Person],
      like: (Person, Person) => Boolean,
      k: Int
  )

  def inputGenerator(
      groupSize: Int,
      likeDislikeRatio: Double,
      pairLikeRatio: Double,
      k: Int,
      sampleSize: Int
  ): List[Input] = {
    (for (n <- 0 until sampleSize) yield {
      val group           = Person.randomGroup(groupSize)
      val personsLiked    = group.take((groupSize * likeDislikeRatio).intValue)
      val personsDisliked = group.drop((groupSize * likeDislikeRatio).intValue)
      def like            = group.randomLikeFunction(pairLikeRatio)

      Input(group, personsLiked, personsDisliked, like, k)
    }).toList
  }

  def si4(
      persons: Set[Person],
      personsLiked: Set[Person],
      personsDisliked: Set[Person],
      like: (Person, Person) => Boolean,
      k: Int
  ): Set[Person] = {

    // Input must satisfy these constraints, or program halts.
    require(personsLiked <= persons, "personsLiked must be a subset of persons")
    require(personsDisliked <= persons, "personsDisliked must be a subset of persons")
    require(
      personsLiked /\ personsDisliked == Set.empty,
      "intersection between personsLiked and personsDisliked must be emtpy"
    )
    require(
      personsLiked \/ personsDisliked == persons,
      "union of personsLiked and personsLiked must equal persons"
    )

    // Specify that invitees is valid if |G /\ D| <= k.
    def atMostKDislikes(invitees: Set[Person]): Boolean =
      (invitees /\ personsDisliked).size <= k

    // Specify the optimality condition.
    def xg(invitees: Set[Person]): Int = {
      val x = invitees.uniquePairs // From all pairs of invitees,
        .build(like.tupled) // select all pairs that like each other,
        .size               // and count them.
      val g = invitees.size // Count the number of total invitees.
      x + g
    }

    val invitees = powerset(persons) // From all possible subsets of persons,
      .build(atMostKDislikes) // select subsets that contain at most k disliked persons,
      .argMax(xg)             // and select the subsets that maximize the optimality condition.

    // If more than one solution exists, return one at random. Always 1 solution must exist,
    // because the empty set is a valid solution. Hence, we can assume random does not
    // return None and 'get' the value.
    invitees.random.get
  }

  def si5(
      persons: Set[Person],
      personsLiked: Set[Person],
      personsDisliked: Set[Person],
      like: (Person, Person) => Boolean
  ): Set[Person] = {

    // Input must satisfy these constraints, or program halts.
    require(personsLiked <= persons, "personsLiked must be a subset of persons")
    require(personsDisliked <= persons, "personsDisliked must be a subset of persons")
    require(
      personsLiked /\ personsDisliked == Set.empty,
      "intersection between personsLiked and personsDisliked must be emtpy"
    )
    require(personsLiked \/ personsDisliked == persons, "union of personsLiked and personsLiked")

    // Specify the optimality condition.
    def gl_x_g(invitees: Set[Person]): Int = {
      val gl = (invitees /\ personsLiked).size // Count the invitees the host likes.
      val x = invitees.uniquePairs // From all pairs of invitees,
        .build(like.tupled) // select all pairs that like each other,
        .size               // and count them.
      val g = invitees.size // Count the number of total invitees.
      gl + x + g
    }

    val invitees = powerset(persons) // From all possible subsets of persons,
      .argMax(gl_x_g) // select those that maximize |G/\L| + |X| + |G|

    // If more than one solution exists, return one at random. Always 1 solution must exist,
    // because the empty set is a valid solution. Hence, we can assume random does not
    // return None and 'get' the value.
    invitees.random.get
  }

  def si6(
      persons: Set[Person],
      personsLiked: Set[Person],
      personsDisliked: Set[Person],
      like: (Person, Person) => Boolean,
      k: Int
  ): Set[Person] = {

    // Input must satisfy these constraints, or program halts.
    require(personsLiked <= persons, "personsLiked must be a subset of persons")
    require(personsDisliked <= persons, "personsDisliked must be a subset of persons")
    require(
      personsLiked /\ personsDisliked == Set.empty,
      "intersection between personsLiked and personsDisliked must be emtpy"
    )
    require(personsLiked \/ personsDisliked == persons, "union of personsLiked and personsLiked")

    // Specify that invitees is valid if |Y| <= k.
    def atMostKPairDislikes(invitees: Set[Person]): Boolean = {
      invitees.uniquePairs | like.tupled
    }.size <= k

    // Specify the optimality condition.
    def gl_g(invitees: Set[Person]): Int = {
      val gl = (invitees /\ personsLiked).size // Count the invitees the host likes.
      val g  = invitees.size                   // Count the number of total invitees.
      gl + g
    }

    val invitees = { powerset(persons) | atMostKPairDislikes _ }
      .argMax(gl_g)

    // If more than one solution exists, return one at random. Always 1 solution must exist,
    // because the empty set is a valid solution. Hence, we can assume random does not
    // return None and 'get' the value.
    invitees.random.get
  }
}

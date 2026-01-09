package mathlibrepo.coherence

import mathlib.graph.{WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._
import mathlibrepo.coherence.Belief.Belief

case class HotBeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    valences: Map[Belief, Double]
) extends BaseBeliefNetwork {

  override protected def cohMin(assignment: TruthValueAssignment): Double = {
    sum(
      { positiveConstraints | isSatisfiedPositiveConstraint(assignment) _ },
      (edge: WUnDiEdge[Belief]) => {
        val leftAct  = if (assignment(edge.left).get) 1.0 else -1.0
        val rightAct = if (assignment(edge.right).get) 1.0 else -1.0
        edge.weight + edge.weight * leftAct * valences(
          edge.left
        ) + edge.weight * rightAct * valences(edge.right)
      }
    )
  }

  override protected def cohPlus(assignment: TruthValueAssignment): Double = {
    sum(
      { negativeConstraints | isSatisfiedNegativeConstraint(assignment) _ },
      (edge: WUnDiEdge[Belief]) => {
        val leftAct  = if (assignment(edge.left).get) 1.0 else -1.0
        val rightAct = if (assignment(edge.right).get) 1.0 else -1.0
        edge.weight + edge.weight * leftAct * valences(
          edge.left
        ) + edge.weight * rightAct * valences(edge.right)
      }
    )
  }
}

import mathlib.graph.GraphImplicits._
import mathlib.graph.{WUnDiEdge, WUnDiGraph}
import mathlibrepo.coherence.Belief.Belief
import mathlibrepo.coherence.{Belief, BeliefNetwork, HotBeliefNetwork}

val edges = Set[WUnDiEdge[Belief]](
  "a" ~ "b" % 1.0,
  "b" ~ "c" % -1.0,
  "a" ~ "c" % 1.0
)

val graph = WUnDiGraph(edges)

val negativeConstraints = Set(
  "b" ~ "c" % .6
)

val beliefNetwork = BeliefNetwork(
  graph,
  negativeConstraints
)

println("ColdCoh")
beliefNetwork.coherenceSolutions().foreach(println)

val valences = Map(
  Belief("a") -> -.4,
  Belief("b") -> .1,
  Belief("c") -> .9
)

val hotBeliefNetwork = HotBeliefNetwork(
  graph,
  negativeConstraints,
  valences
)

println("HotCoh")
hotBeliefNetwork.coherenceSolutions().foreach(println)
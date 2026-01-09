package mathlibrepo.categorization

import mathlib.set.SetTheory._

object Categorization {
  def categorization(
      objects: Set[String],
      sim: (String, String) => Double,
      dis: (String, String) => Double
  ): Set[Set[Set[String]]] = {
    def withinSim(category: Set[String]): Double =
      sum(category.combinations2, sim)

    def betweenDis(
        categoryA: Set[String],
        categoryB: Set[String]
    ): Double =
      sum(categoryA x categoryB, dis)

    def fun(partition: Set[Set[String]]): Double =
      sum(partition, withinSim _) +
        sum(partition.combinations2, betweenDis _)

    val allPartitions = objects.allPartitions
    argMax(allPartitions, fun)
  }

//  def inputToDotString(
//      objects: Set[String],
//      sim: (String, String) => Double,
//      dis: (String, String) => Double
//  ): String = {
//    "graph{" +
//      objects.combinations2
//        .map {
//          case (a: String, b: String) => {
//            s"$a--$b[color=orange,penwidth=${5 * sim(a, b)}];$a--$b[color=purple,penwidth=${5 * dis(a, b)}];"
//          }
//        }
//        .mkString("") +
//      "}"
//  }
//
//
////  graph{
////
////    compound=true;
////
////    subgraph cluster0 {
////      peripheries=1
////      a
////      b
////    };
////    subgraph cluster1 {
////      peripheries=1
////      c
////    };
////
////    a--b[color=orange,penwidth=5.0];
////    a--b[color=purple,penwidth=5.0];
////    a--c[color=orange,penwidth=0.5];
////    a--c[color=purple,penwidth=1.0];
////    b--c[color=orange,penwidth=0.5];
////    b--c[color=purple,penwidth=0.5];
////  }
//  def categorizationToDotString(
//      partition: Set[Set[String]],
//      sim: (String, String) => Double,
//      dis: (String, String) => Double
//  ): String = {
//    val objects = partition.flatten
//    "graph{" +
//      objects.combinations2
//        .map {
//          case (a: String, b: String) => {
//            s"$a--$b[color=orange,penwidth=${5 * sim(a, b)}];$a--$b[color=purple,penwidth=${5 * dis(a, b)}];"
//          }
//        }
//        .mkString("") +
//      "}"
//  }
}

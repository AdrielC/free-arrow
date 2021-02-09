# Information Retrieval and Ranking Evaluation

### Supported Metrics

- Ndcg
- Reciprocal Rank
- FScore/F1
- QMeasure (variant of AP)
- R-Precision
- Precision
- Recall


### Quickstart

```scala

sealed trait Eng // user defined engagement domain
case object Clicks extends Eng

type MyResults = (NonEmptyList[ResultId], Map[ResultId, Map[Eng, Int]]) // user defined results type

val results: MyResults = (
  NonEmptyList.of(1L, 2L to 59L:_*), // results
  Map(2L -> Map((Clicks: Eng) -> 1)) // engagement counts
)

import dsl._

val evaluation =
  label.of(Clicks: Eng).from[MyResults] >>> // count clicks to determine relevance labels
    atK(10, 60) >++                               // compute downstream metrics for each K
    (eval.ndcg, eval.reciprocalRank)              // compute each metric

val metrics = evaluation.run(results)

metrics == NonEmptyMap.of(
  "label(clicks).mrr.@10" -> Right(0.5),
  "label(clicks).ndcg.@10" -> Right(0.6309297535714574),
  "label(clicks).@60" -> Left(KGreaterThanMax: EvalErr) // metric key stops building on error so Errors aren't repeated for all downstream metric combinations
)
```

# Walkthrough

```scala

 // your own class
 case class MyClass(results: List[Long], clicks: List[Long], cartAdds: List[Long], purchases: List[Long])
 object MyClass {
  sealed trait Eng
  case object Clicks extends Eng
  case object Reviews extends Eng
  implicit val engagementsInstance: quivr.metrics.Engagements[MyClass, Eng] = ???
 }

 import dsl._

 // define building blocks for evaluation
```
 The four building blocks are
 1) `Labeler[E]`  : Defines how to create continuous/graded labels from your data, `E` being the label source
 2) `Judge[E]`    : Defines how to create binary relevance judgements from your data
 3) `A >> B`      : Represents a single step in your pipeline that inputs `A` and outputs `B`
 4) `A +> B`      : Represents multiple `A >> B` arrows that have been batched together, still having inputs `A` and outputs `B`
 
```scala
 // define how you want to extract your labels
 val countClicks    = label.of(Clicks)    : Labeler[Eng]
 val countPurchases = label.of(Reviews)   : Labeler[Eng]
 val anyClicks      = judge.any(Clicks)   : Judge[Eng]
 val anyPurchases   = judge.any(Purchases): Judge[Eng]

 // specify which datatype will be required for input

 val singleLabeler: MyClass >> WithLabels[MyClass]  = countClicks.from[MyClass]   // for one labeler

 val manyLabelers: MyClass +> WithLabels[MyClass]   = label[MyClass](  // or for many labelers
  countClicks,
  countPurchases,
  countClicks + (countPurchases * 5)
 )

 // specify values of k to compute metrics by
 val atKs: WithLabels[MyClass] +> WithLabels[MyClass] = atK(10, 20, 30, 40, 60)

 // Build an evaluator: Compose via `>>>`, combine via `<+>`, and compose/combine in a single step via `>++`

 val labelEval = (manyLabelers <+> singleLabeler) >>> atKTen >>> eval.ndcg

 val judgeEval = anyClicks >>> atK(5, 10, 40, 90) >++ (eval.precision, eval.recall, eval.averagePrecision)

 val fullEvaluation: MyClass +> Double = labelEval <+> judgeEval

 // You can also map ordinary functions into the structure via `rmap`, `lmap`

 val legacyEvaluation: OtherClass +> Double = fullEvaluation.lmap((_: OtherClass).toMyclass)

 val metrics: NonEmptyMap[String, EvalResult[Double]] = legacyEvaluation.run(otherClass) // Each metric will be accounted for, failure (e.g. no results) or not

 val metric: (String, EvalResult[Double]) = (singleLabeler >>> eval.ndcg).run(myClass) // Or just compute one metric.
```

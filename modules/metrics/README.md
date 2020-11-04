# Information Retrieval and Ranking Evaluation


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
  label.count.of(Clicks: Eng).from[MyResults] >>> // count clicks to determine relevance labels
    atK(10, 60) >++                               // compute downstream metrics for each K
    (eval.ndcg, eval.reciprocalRank)              // compute each metric

val metrics = evaluation.run(results)

metrics == NonEmptyMap.of(
  "label(clicks).mrr.@10" -> Right(0.5),
  "label(clicks).ndcg.@10" -> Right(0.6309297535714574),
  "label(clicks).@60" -> Left(KGreaterThanMax: EvalErr) // metric key stops building on error so Errors aren't repeated for all downstream metric combinations
)
```
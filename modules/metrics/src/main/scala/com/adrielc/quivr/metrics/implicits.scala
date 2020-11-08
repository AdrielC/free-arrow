package com.adrielc.quivr.metrics

object implicits
  extends result.AtK.ToAtKOps
  with ranking.RankedRelevancies.ToRankedRelevanciesOps
  with ranking.ResultRelevancies.ToResultRelevanciesOps
  with result.Results.ToResultsOps
  with result.Engagements.ToEngagementsOps
  with result.GroundTruth.ToGroundTruthOps
  with retrieval.TruePositiveCount.ToTruePositiveCountOps
  with result.Relevancy.ToRelevancyOps
  with retrieval.RelevanceCount.ToRelevanceCountOps
  with result.ResultLabels.ToResultLabelsOps

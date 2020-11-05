package com.adrielc.quivr.metrics.data

import com.adrielc.quivr.metrics.result.Results

object relevance {

  sealed trait Relevance {
    import Relevance._
    def gain: Option[Gain] = this match {
      case Relevance.Unjudged => None
      case Irrelevant         => Some(Gain.zero)
      case BinaryJudgement(r) => Some(if(r) Gain.one else Gain.zero)
      case GradedLabel(l)     => Some(l)
    }
  }
  object Relevance {

    def from(value: Double): Relevance = Label.from(value).fold(_ => Irrelevant, GradedLabel)
    def label(label: Label): Relevance = GradedLabel(label)
    def binary(isRel: Boolean): Relevance = BinaryJudgement(isRel)
    val unjudged: Relevance = Unjudged
    val irrelevant: Relevance = Irrelevant

    private final case class GradedLabel(label: Label)            extends Relevance
    private final case class BinaryJudgement(isRelevant: Boolean) extends Relevance
    private case object Irrelevant                                extends Relevance
    private case object Unjudged                                  extends Relevance



    // guarantees that if a list of relevancies is returned then there is at least one judged result
    def fromLabelsToRel[A: Results, V](a: A, labels: Map[ResultId, V], judgeLabel: V => Relevance): Option[ResultRels] = {
      val rels = labels.mapValues(judgeLabel)
      val results = Results[A].results(a).map(id => id -> rels.getOrElse(id, Relevance.unjudged))

      ResultRels(results, rels.count(_._2.isRel))
    }
  }


  // Frequently used relevance scale in IR Evaluation
  sealed trait SABRel extends Product with Serializable {
    def gain: Option[Gain] = this match {
      case j: SABRel.Judged[_]  => Some(j.label)
      case SABRel.Unjudged             => None
    }
  }
  object SABRel {
    import shapeless.{Nat, nat, ops}, ops.nat.ToInt, nat._

    sealed abstract class Judged[N <: Nat: ToInt] extends SABRel {
      lazy val label: Label = Label.unsafeFrom(ToInt[N].apply().toDouble)
    }
    object Judged {
      case object SRel extends Judged[_3] // Highly Relevant
      case object ARel extends Judged[_2] // Relevant
      case object BRel extends Judged[_1] // Partially Relevant
      case object NRel extends Judged[_0] // Non Relevant

      val S: Judged[_3] = SRel
      val A: Judged[_2] = ARel
      val B: Judged[_1] = BRel
      val N: Judged[_0] = NRel
    }
    case object Unjudged extends SABRel
  }
}

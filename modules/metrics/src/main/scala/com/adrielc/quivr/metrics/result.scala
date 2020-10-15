package com.adrielc.quivr.metrics

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import simulacrum.{op, typeclass}
import cats.implicits._
import com.adrielc.quivr.metrics.data.{Label, ResultId}


@typeclass trait ResultSet[A] extends ResultCount[A] {

  @op("results")
  def results(a: A): NonEmptyList[ResultId]

  def withGroundTruth(a: A, groundTruth: NonEmptySet[ResultId]): NonEmptyList[Boolean] =
    results(a).map(groundTruth.contains)

  def labelWithLabels(a: A, labels: NonEmptyMap[ResultId, Label]): NonEmptyList[Double] =
    results(a).map(labels.lookup(_).getOrElse(0.0))

  override def resultCount(a: A): Int =
    results(a).size
}
object ResultSet {
  implicit val resultSet: ResultSet[NonEmptyList[ResultId]] = identity
  implicit def leftTupleResultSet[A: ResultSet, B]: ResultSet[(A, B)] = a => ResultSet[A].results(a._1)
  implicit def rightTupleResultSet[A, B: ResultSet]: ResultSet[(A, B)] = a => ResultSet[B].results(a._2)
}

@typeclass trait GroundTruthSet[A] extends GroundTruthCount[A] {

  @op("groundTruth")
  def groundTruth(a: A): NonEmptySet[ResultId]

  override def groundTruthCount(a: A): Int =
    groundTruth(a).size.toInt
}
object GroundTruthSet {
  implicit val groundTruthSetIdentityInstance: GroundTruthSet[NonEmptySet[ResultId]] = identity
  implicit def groundTruthSetTupleLeftInstance[A: GroundTruthSet, B]: GroundTruthSet[(A, B)] = a => GroundTruthSet[A].groundTruth(a._1)
  implicit def groundTruthSetTupleRightInstance[A, B: GroundTruthSet]: GroundTruthSet[(A, B)] = a => GroundTruthSet[B].groundTruth(a._2)
}

@typeclass trait ResultLabels[A] extends Serializable {

  @op("resultLabels")
  def resultLabels(a: A): NonEmptyMap[ResultId, Label]
}
object ResultLabels {
  implicit val resultLabelsIdentityInstance: ResultLabels[NonEmptyMap[ResultId, Label]] = identity
  implicit def resultLabelsTupleLeftInstance[A: ResultLabels, B]: ResultLabels[(A, B)] = a => ResultLabels[A].resultLabels(a._1)
  implicit def resultLabelsTupleRightInstance[A, B: ResultLabels]: ResultLabels[(A, B)] = a => ResultLabels[B].resultLabels(a._2)
}

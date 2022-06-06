package com.adrielc.quivr.aggregator

import cats.Monoid
import cats.implicits._
import cats.kernel.Order

case class AveragedValue(value: Double, count: Long = 1) {

  def average: Double = if(count == 0) 0 else value / count
}

object AveragedValue {

  def apply[N](n: N)(implicit N: Numeric[N]): AveragedValue = new AveragedValue(N.toDouble(n))

  implicit val numericAverage: Numeric[AveragedValue] = new Numeric[AveragedValue] {
    def toDouble(x: AveragedValue): Double = x.average
    def toInt(x: AveragedValue): Int = toDouble(x).toInt
    def toFloat(x: AveragedValue): Float = toDouble(x).toFloat
    def toLong(x: AveragedValue): Long = toDouble(x).toLong
    def fromInt(x: Int): AveragedValue = AveragedValue(x)
    def negate(x: AveragedValue): AveragedValue = AveragedValue(-x.value, x.count)
    def times(x: AveragedValue, y: AveragedValue): AveragedValue = AveragedValue(x.value * y.value, x.count + y.count)
    def minus(x: AveragedValue, y: AveragedValue): AveragedValue = AveragedValue(x.value - y.value, x.count + y.count)
    def plus(x: AveragedValue, y: AveragedValue): AveragedValue = monoidAvg.combine(x, y)
    def compare(x: AveragedValue, y: AveragedValue): Int = averageOrder.compare(x, y)
  }

  implicit val monoidAvg: Monoid[AveragedValue] = Monoid.instance(AveragedValue(0, 0), (a, b) => AveragedValue(a.value + b.value, a.count + b.count))

  implicit val averageOrder: Order[AveragedValue] = Order.by(_.count)

  implicit val averageValueAgg: MonoidAggregator.Aux[AveragedValue, AveragedValue, AveragedValue] = MonoidAggregator.instanceMonoid

  def averageOf[N : Numeric]: MonoidAggregator.Aux[N, Double, AveragedValue] = averageValueAgg.dimap(AveragedValue(_), _.average)

  def averageOf[A, N : Numeric](f: A => N): MonoidAggregator.Aux[A, Double, AveragedValue] = averageOf[N].contramap(f)

  def roundedAverageOf[N](implicit N: Numeric[N]): MonoidAggregator.Aux[N, N, AveragedValue] = averageOf[N].map(v => N.fromInt(v.round.toInt))

  def roundedAverageOf[A, N : Numeric](f: A => N): MonoidAggregator.Aux[A, N, AveragedValue] = roundedAverageOf[N].contramap(f)
}
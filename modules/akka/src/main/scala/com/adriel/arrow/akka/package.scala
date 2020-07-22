package com.adriel.arrow

import _root_.akka.NotUsed
import _root_.akka.stream.FlowShape
import _root_.akka.stream.scaladsl.{Flow, Broadcast, Concat, GraphDSL, Sink, Source, Unzip, Zip}
import com.adrielc.arrow.ArrowChoicePlus

package object akka {

  type PureFlow[-A, +B] = Flow[A, B, NotUsed]

  implicit def flowArrow[M]: ArrowChoicePlus[PureFlow] = new ArrowChoicePlus[PureFlow] {

    override def choose[A, B, C, D](f: PureFlow[A, C])(g: PureFlow[B, D]): PureFlow[Either[A, B], Either[C, D]] =
      Flow[Either[A, B]].flatMapConcat {
        case Left(value)  => Source.single(value).via(f).map(Left(_))
        case Right(value) => Source.single(value).via(g).map(Right(_))
      }

    def plus[A, B](f: PureFlow[A, B], g: PureFlow[A, B]): PureFlow[A, B] =
      Flow.fromGraph(GraphDSL.create() { implicit builder =>
        import GraphDSL.Implicits._

        val bcast = builder.add(Broadcast[A](2))

        val cncat = builder.add(Concat[B](2))

        bcast ~> f ~> cncat.in(1)
        bcast ~> g ~> cncat.in(2)

        FlowShape(bcast.in, cncat.out)
      })

    def zeroArrow[B, C]: PureFlow[B, C] =
      Flow.fromSinkAndSourceCoupled(Sink.ignore, Source.empty)

    def lift[A, B](f: A => B): PureFlow[A, B] =
      Flow.fromFunction(f)

    override def id[A]: PureFlow[A, A] =
      Flow[A]

    def compose[A, B, C](f: PureFlow[B, C], g: PureFlow[A, B]): PureFlow[A, C] =
      g.via(f)

    def first[A, B, C](fa: PureFlow[A, B]): PureFlow[(A, C), (B, C)] =
      Flow.fromGraph(GraphDSL.create() { implicit builder =>

        import GraphDSL.Implicits._

        val uz = builder.add(Unzip[A, C])
        val zu = builder.add(Zip[B, C])

        uz.out0 ~> fa       ~>  zu.in0
        uz.out1 ~> Flow[C]  ~>  zu.in1

        FlowShape(uz.in, zu.out)
      })
  }
}

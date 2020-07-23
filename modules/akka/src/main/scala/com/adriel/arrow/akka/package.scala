package com.adriel.arrow

import _root_.akka.NotUsed
import _root_.akka.stream.FlowShape
import _root_.akka.stream.scaladsl.{Concat, Broadcast, Merge, Flow, GraphDSL, Keep, Partition, Sink, Source, Unzip, Zip}
import com.adrielc.arrow.ArrowChoicePlus
import cats.implicits._

package object akka {

  type PureFlow[-A, +B] = Flow[A, B, NotUsed]

  implicit def flowArrow[M]: ArrowChoicePlus[PureFlow] = new ArrowChoicePlus[PureFlow] {

    def choose[A, B, C, D](f: PureFlow[A, C])(g: PureFlow[B, D]): PureFlow[Either[A, B], Either[C, D]] =
      Flow.fromGraph(GraphDSL.create(f, g)(Keep.none) { implicit b => (l, r) =>
        import GraphDSL.Implicits._

        val partition = b.add(Partition[Either[A, B]](2, _.fold(_ => 0, _ => 1)))

        val merge = b.add(Merge[Either[C, D]](2))

        partition.out(0).map(_.left.get)  ~> l.in
        partition.out(1).map(_.right.get) ~> r.in

        l.out.map(_.asLeft)   ~> merge.in(0)
        r.out.map(_.asRight)  ~> merge.in(1)

        FlowShape(partition.in, merge.out)
      })

    def plus[A, B](f: PureFlow[A, B], g: PureFlow[A, B]): PureFlow[A, B] =
      Flow.fromGraph(GraphDSL.create() { implicit builder =>
        import GraphDSL.Implicits._
        val bcast = builder.add(Broadcast[A](2))
        val cncat = builder.add(Concat[B]())
        bcast ~> f ~> cncat.in(0)
        bcast ~> g ~> cncat.in(1)
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
      Flow.fromGraph(GraphDSL.create(fa) { implicit builder => fst =>
        import GraphDSL.Implicits._

        val uz = builder.add(Unzip[A, C])
        val zu = builder.add(Zip[B, C])

        uz.out0 ~> fst     ~>  zu.in0
        uz.out1 ~> Flow[C] ~>  zu.in1

        FlowShape(uz.in, zu.out)
      })

    override def split[A, B, C, D](f: PureFlow[A, B], g: PureFlow[C, D]): PureFlow[(A, C), (B, D)] =
      Flow.fromGraph(GraphDSL.create(f, g)(Keep.none) { implicit builder => (fst, snd) =>
        import GraphDSL.Implicits._

        val uz = builder.add(Unzip[A, C])
        val zu = builder.add(Zip[B, D])

        uz.out0 ~> fst ~>  zu.in0
        uz.out1 ~> snd ~>  zu.in1

        FlowShape(uz.in, zu.out)
      })

    override def second[A, B, C](fa: PureFlow[A, B]): PureFlow[(C, A), (C, B)] =
      Flow.fromGraph(GraphDSL.create(fa) { implicit builder => snd =>
        import GraphDSL.Implicits._

        val uz = builder.add(Unzip[C, A])
        val zu = builder.add(Zip[C, B])

        uz.out0 ~> Flow[C]  ~>  zu.in0
        uz.out1 ~> snd      ~>  zu.in1

        FlowShape(uz.in, zu.out)
      })
  }
}

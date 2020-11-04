package akka.stream

import akka.NotUsed
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, OrElse, Partition, Sink, Source, Unzip, WireTap, Zip}
import com.adrielc.quivr.ArrowChoiceZero
import cats.implicits._

package object quivr {

  type AkkaFlow[-A, +B] = Flow[A, B, NotUsed]

  implicit def flowArrow[M]: ArrowChoiceZero[AkkaFlow] = new ArrowChoiceZero[AkkaFlow] {

    def lift[A, B](f: A => B): AkkaFlow[A, B] =
      Flow.fromFunction(f)

    override def id[A]: AkkaFlow[A, A] =
      Flow[A]

    def compose[A, B, C](f: AkkaFlow[B, C], g: AkkaFlow[A, B]): AkkaFlow[A, C] =
      g.via(f)

    def zeroArrow[B, C]: AkkaFlow[B, C] =
      emptyFlow

    override def and[A, B, C](f: AkkaFlow[A, B], g: AkkaFlow[A, C]): AkkaFlow[A, Either[B, C]] =
      Flow.fromGraph(GraphDSL.create(
        f.map(_.asLeft[C]),
        g.map(_.asRight[B])
      )(Keep.none) { implicit builder => (first, second) =>
        import GraphDSL.Implicits._

        val broadcast = builder.add(WireTap[A])
        val merge     = builder.add(Merge[Either[B, C]](inputPorts = 2))


        broadcast.out0 ~>   first   ~> merge.in(0)
        broadcast.out1 ~>   second  ~> merge.in(1)


        FlowShape(broadcast.in, merge.out)
      })

    def choose[A, B, C, D](f: AkkaFlow[A, C])(g: AkkaFlow[B, D]): AkkaFlow[Either[A, B], Either[C, D]] =
      Flow.fromGraph(GraphDSL.create(
        dimap(f)((_: Either[A, B]).left.get)(_.asLeft[D]),
        dimap(g)((_: Either[A, B]).right.get)(_.asRight[C])
      )(Keep.none) { implicit b => (left, right) =>
        import GraphDSL.Implicits._

        val partition = b.add(Partition[Either[A, B]](2, _.fold(_ => 0, _ => 1)))
        val merge     = b.add(Merge[Either[C, D]](2))

        partition.out(0) ~>  left   ~> merge.in(0)
        partition.out(1) ~>  right  ~> merge.in(1)

        FlowShape(partition.in, merge.out)
      })

    def plus[A, B](f: AkkaFlow[A, B], g: AkkaFlow[A, B]): AkkaFlow[A, B] = {

      val (first, second) = if(f == emptyFlow) (g, f) else (f, g)

      Flow.fromGraph(GraphDSL.create(first, second)(Keep.none) { implicit builder => (main, backup) =>
        import GraphDSL.Implicits._

        val balance   = builder.add(Broadcast[A](2))
        val orElse    = builder.add(OrElse[B])


        balance.out(0) ~>   main    ~> orElse.in(0)
        balance.out(1) ~>   backup  ~> orElse.in(1)


        FlowShape(balance.in, orElse.out)
      })
    }

    def first[A, B, C](fa: AkkaFlow[A, B]): AkkaFlow[(A, C), (B, C)] =
      Flow.fromGraph(GraphDSL.create(fa, Flow[C])(Keep.none) { implicit builder => (first, second) =>
        import GraphDSL.Implicits._

        val unzip = builder.add(Unzip[A, C])
        val zip   = builder.add(Zip[B, C])


        unzip.out0 ~>  first    ~> zip.in0
        unzip.out1 ~>  second   ~> zip.in1


        FlowShape(unzip.in, zip.out)
      })
  }

  private val emptyFlow: AkkaFlow[Any, Nothing] = Flow.fromSinkAndSourceCoupled(Sink.ignore, Source.empty)
}

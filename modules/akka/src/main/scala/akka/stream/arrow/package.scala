package akka.stream

import _root_.akka.NotUsed
import _root_.akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, OrElse, Partition, Sink, Source, Unzip, Zip}
import com.adrielc.arrow.ArrowChoicePlus
import cats.implicits._

package object arrow {

  type FlowN[-A, +B] = Flow[A, B, NotUsed]

  implicit def flowArrow[M]: ArrowChoicePlus[FlowN] = new ArrowChoicePlus[FlowN] {

    def choose[A, B, C, D](f: FlowN[A, C])(g: FlowN[B, D]): FlowN[Either[A, B], Either[C, D]] =
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

    def plus[A, B](f: FlowN[A, B], g: FlowN[A, B]): FlowN[A, B] = {

      val (first, second) = if(f == emptyFlow) (g, f) else (f, g)

      Flow.fromGraph(GraphDSL.create(first, second)(Keep.none) { implicit builder => (a, b) =>
        import GraphDSL.Implicits._
        val tap = builder.add(Broadcast[A](2))
        val orElse = builder.add(OrElse[B]())
        tap.out(0) ~> a ~> orElse.in(0)
        tap.out(1) ~> b ~> orElse.in(1)
        FlowShape(tap.in, orElse.out)
      })
    }

    def zeroArrow[B, C]: FlowN[B, C] =
      emptyFlow

    def lift[A, B](f: A => B): FlowN[A, B] =
      Flow.fromFunction(f)

    override def id[A]: FlowN[A, A] =
      Flow[A]

    def compose[A, B, C](f: FlowN[B, C], g: FlowN[A, B]): FlowN[A, C] =
      g.via(f)

    def first[A, B, C](fa: FlowN[A, B]): FlowN[(A, C), (B, C)] =
      Flow.fromGraph(GraphDSL.create(fa) { implicit builder => fst =>
        import GraphDSL.Implicits._

        val uz = builder.add(Unzip[A, C])
        val zu = builder.add(Zip[B, C])

        uz.out0 ~> fst     ~>  zu.in0
        uz.out1 ~> Flow[C] ~>  zu.in1

        FlowShape(uz.in, zu.out)
      })
  }

  private val emptyFlow: FlowN[Any, Nothing] = Flow.fromSinkAndSourceCoupled(Sink.ignore, Source.empty)
}

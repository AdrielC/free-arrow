package akka.stream.arrow

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import com.adrielc.arrow.free.FreeA._
import Recs._
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import com.adrielc.arrow.free.FCP

class AkkaStreamArrowSpec extends FlatSpec with Matchers {

  private def createFlow(counter: AtomicInteger): FCP[Recs, User, Either[Unit, Either[Unit, Product]]] = {

    val handleError = lift(logError) >>> always(counter.getAndIncrement()) >>^ (i => println((i + 1) + " errors"))

    val getRecsAndSend = getRecommendations >>> lift(getTopRecommend) >>> (handleError +++ sendRecommend)

    (getUserInfo >>> needsRecommendation).test >>> ((handleError <<^ InvalidUser) +++ getRecsAndSend)
  }


  "FreeA" should "interpret to Function" in {

    val errorCounter = new AtomicInteger(0)

    val flow = createFlow(errorCounter)

    // Interpret flow as pure function and test with one user
    val kleisli = flow.foldMap(Recs.~~>.pure.kleisli[List])


    val sentProducts =
      Recs.~~>.userStream(10)
        .toList
        .foldMap(kleisli.run)
        .collect { case Right(Right(p)) => p }

    assert(sentProducts == List(
      Product(-8962818485898350288L),
      Product(-8766919991080115071L),
      Product(-8962818485898350288L),
      Product(-7733313153529842875L),
      Product(-4518701576662962201L)
    ))

    assert(errorCounter.get() == 5)
  }


  "FreeA.plus" should "fall back to second" in {

    implicit val system: ActorSystem = ActorSystem.create("PlusTest")

    val errorCounter = new AtomicInteger(0)

    val flow = {

      val f = createFlow(errorCounter)

      zeroArrow <+> ((f |&| f) >>> justRight)
    }

    flow
      .foldMap(Recs.~~>.toAkka)
      .runWith(
        Recs.~~>.usersSource(20),
        Sink.foreach(println)
      )

    system.registerOnTermination(
      assert(errorCounter.get() == 15)
    )
  }
}

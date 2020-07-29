package akka.stream.arrow

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import com.adrielc.arrow.free.FreeA._

import cats.instances.all._

import Recs._

object Main extends App {
  implicit val system: ActorSystem = ActorSystem.create("QuickStart")

  val handleError = lift(logError)

  val getRecsAndSend = getRecommendations >>> lift(getTopRecommend) >>> (handleError ||| sendRecommend)

  val flow = (getUserInfo >>> needsRecommendation).test >>> (handleError <<^ InvalidUser ||| getRecsAndSend)

  // Interpret flow as pure function and test with one user
  flow
    .foldMap(Recs.~~>.pure.kleisli[Option])
    .run(Recs.~~>.getUser)


  // Interpret flow as stream and run
  flow
    .foldMap(Recs.~~>.toAkka)
    .runWith(
      Recs.~~>.usersInfSource,
      Sink.ignore
    )
}

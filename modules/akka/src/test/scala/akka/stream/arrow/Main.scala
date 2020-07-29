package akka.stream.arrow

import java.util.logging.Logger

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import com.typesafe.config.ConfigFactory

import cats.instances.all._

object Main extends App {

  val logger = Logger.getGlobal


  implicit val system: ActorSystem = {
    val classLoader = getClass.getClassLoader
    ActorSystem("QuickStart", ConfigFactory.load(classLoader), classLoader)
  }

  val flow = {
    import com.adrielc.arrow.free.FreeA._
    import Recs._

    val handleError = lift(logError)

    val getRecsAndSend =
      getRecommendations >>> lift(getTopRecommend) >>> (handleError ||| sendRecommend)

    val flow =
      (getUserInfo >>> needsRecommendation).test >>> (handleError.lmap(InvalidUser) ||| getRecsAndSend)

    flow <+> zeroArrow
  }

  // Interpret flow as pure function and test with one user
  flow
    .foldMap(Recs.~~>.pure.kleisli[Option])
    .run(Recs.~~>.getUser)


  // Interpret flow as stream and run
  flow
    .foldMap(Recs.~~>.toAkka)
    .recover { case e: Throwable => logger.warning(e.getMessage) }
    .runWith(
      Recs.~~>.usersInfSource,
      Sink.ignore
    )
}

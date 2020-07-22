package com.adriel.arrow.akka

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.adrielc.arrow.~~>
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class AkkaArrowSpec extends FlatSpec with Matchers {
  import Recs._
  import com.adrielc.arrow.free.FreeA._

  implicit val sys: ActorSystem = ActorSystem()

  case class User(id: Long)
  case class Product(id: Long)
  case class ProductInfo(id: Long, price: Double)
  case class UserInfo(id: Long, isMember: Boolean)

  sealed trait Recs[A, B]
  object Recs {

    case object GetUserInfo         extends Recs[User, UserInfo]
    case object GetRecommendations  extends Recs[User, List[Product]]
    case object GetProductInfo      extends Recs[Product, ProductInfo]
    case object ValidateUser        extends Recs[UserInfo, Boolean]
    case object SendRecommend       extends Recs[Product, Unit]

    sealed trait RecsError
    case class  InvalidUser(user: User) extends RecsError
    case object NoRecs                  extends RecsError

    def getTopRecommend(recs: List[Product]): Either[RecsError, Product] =
      recs.sortBy(_.id).headOption match {
        case Some(value) => scala.Right(value)
        case None =>        scala.Left(NoRecs)
      }

    def logError(error: RecsError): Unit = error match {
      case InvalidUser(user) => println(s"invalid user ${user.id}")
      case NoRecs => println("noRecs")
    }


    val getUserInfo         = lift(GetUserInfo)
    val getProductInfo      = lift(GetProductInfo)
    val getRecommendations  = lift(GetRecommendations)
    val needsRecommendation = lift(ValidateUser)
    val sendRecommend       = lift(SendRecommend)

    object ~~> {

      private val SEED = 100
      private val rng = new Random(SEED)
      private val users = List.fill(SEED)(User(rng.nextLong()))
      private val userInfo = users.map(u => u.id -> UserInfo(u.id, rng.nextBoolean())).toMap
      private val products = Array.fill(SEED * 10)(Product(rng.nextLong()))
      private val productInfo = products.map(p => p.id -> ProductInfo(p.id, math.abs(rng.nextDouble()) % 100.0)).toMap

      val usersSource = Source(users)

      val toAkka = new (Recs ~~> PureFlow) {

        def apply[A, B](fab: Recs[A, B]): PureFlow[A, B] = fab match {

          case GetRecommendations => Flow[User].map { u =>
            val r = new Random(u.id)
            List.fill(u.id.toInt % 10)(products(r.nextInt() % SEED))
          }

          case GetUserInfo => Flow[User].map(u => userInfo(u.id))

          case GetProductInfo => Flow[Product].map(p => productInfo(p.id))

          case ValidateUser => Flow[UserInfo].map(_.isMember)

          case SendRecommend => Flow[Product].map(p => println(s"sent $p"))
        }
      }
    }
  }

  val freeFlow =
    fn((u: User) => { println(u); u }) >>>
    (getUserInfo >>> needsRecommendation).test >>>
    (fn(InvalidUser.apply _ andThen logError) |||
      getRecommendations >>^
        getTopRecommend >>>
        (fn(logError) ||| sendRecommend))

  val recsFlow = freeFlow.foldMap(Recs.~~>.toAkka)

  val graph =
    Recs.~~>
    .usersSource
    .via(recsFlow)
    .to(Sink.ignore)

  "Graph" should "run" in {

    graph.run()
  }
}


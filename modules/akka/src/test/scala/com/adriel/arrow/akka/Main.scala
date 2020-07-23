package com.adriel.arrow.akka

import java.util.concurrent.atomic.AtomicInteger
import java.util.logging.Logger

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.adrielc.arrow.{Pure, ~~>}
import com.typesafe.config.ConfigFactory
import cats.implicits._

import scala.util.Random

object Main extends App {

  val logger = Logger.getGlobal

  import Recs._
  import com.adrielc.arrow.free.FreeA._

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


    // Smart constructors
    val getUserInfo         = lift(GetUserInfo)
    val getProductInfo      = lift(GetProductInfo)
    val getRecommendations  = lift(GetRecommendations)
    val needsRecommendation = lift(ValidateUser)
    val sendRecommend       = lift(SendRecommend)

    object ~~> {
      private val SEED = 100
      private val rng = new Random(SEED)

      val users: List[User] = List.fill(SEED)(User(rng.nextLong()))

      private val userInfo = users.map(u => u.id -> UserInfo(u.id, rng.nextBoolean())).toMap
      private val products = Array.fill(SEED * 10)(Product(rng.nextLong()))
      private val productInfo = products.map(p => p.id -> ProductInfo(p.id, math.abs(rng.nextDouble()) % 100.0)).toMap
      private val userIdx = new AtomicInteger(0)

      def getUser: User = users(userIdx.incrementAndGet() % SEED)

      def userStream(n: Int): Iterator[User] = Stream.fill(n)(getUser).toIterator

      def usersSource(n: Int): Source[User, NotUsed] = Source.fromIterator(() => userStream(n))

      val toAkka = new (Recs ~~> PureFlow) {

        def apply[A, B](fab: Recs[A, B]): PureFlow[A, B] = fab match {

          case GetRecommendations => Flow[User].map { u =>
            val r = new Random(u.id)
            val prods = List.fill(10)(products(math.abs(r.nextInt() % SEED)))
            prods
          }

          case GetUserInfo => Flow[User].map(u => userInfo(u.id))

          case GetProductInfo => Flow[Product].map(p => productInfo(p.id))

          case ValidateUser => Flow[UserInfo].map(_.isMember)

          case SendRecommend => Flow[Product].map(p => println(s"sent $p"))
        }
      }

      val pure = new Pure[Recs] {
        def apply[A, B](fab: Recs[A, B]): A => B = fab match {
          case GetUserInfo => (u: User) => userInfo(u.id)
          case GetRecommendations => (u: User) => {
            val r = new Random(u.id)
            val prods = List.fill(10)(products(math.abs(r.nextInt() % SEED)))
            prods
          }
          case GetProductInfo => (p: Product) => productInfo(p.id)
          case ValidateUser => ((_: UserInfo).isMember)
          case SendRecommend => p => println(s"sent $p")
        }
      }
    }
  }


  implicit val system: ActorSystem = {
    val classLoader = getClass.getClassLoader
    ActorSystem("QuickStart", ConfigFactory.load(classLoader), classLoader)
  }

  val handleInvalidUser = fn(InvalidUser(_: User)) >>> fn(logError)

  val getRecsAndSend = getRecommendations >>> fn(getTopRecommend) >>> (fn(logError) ||| sendRecommend)

  val freeFlow = (getUserInfo >>> needsRecommendation).test >>> (handleInvalidUser ||| getRecsAndSend)

  val flows = freeFlow <+> freeFlow

  val recsFlow = flows.foldMap(Recs.~~>.toAkka)

  Recs.~~>
    .usersSource(2)
    .via(recsFlow)
    .recover {
      case e: Throwable => logger.warning(e.getMessage)
    }
    .runWith(Sink.ignore)
}

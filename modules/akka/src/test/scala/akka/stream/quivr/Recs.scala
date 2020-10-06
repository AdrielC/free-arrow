package akka.stream.quivr

import java.util.concurrent.atomic.AtomicInteger

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Source}
import com.adrielc.quivr.free.FreeArrow.liftK
import com.adrielc.quivr.{ToFunction, ~~>}

import scala.util.Random

sealed trait Recs[A, B]
object Recs {

  case object GetUserInfo         extends Recs[User, UserInfo]
  case object GetRecommendations  extends Recs[User, List[Product]]
  case object GetProductInfo      extends Recs[Product, ProductInfo]
  case object ValidateUser        extends Recs[UserInfo, Boolean]
  case object SendRecommend       extends Recs[Product, Product]

  case class User(id: Long)
  case class Product(id: Long)
  case class ProductInfo(id: Long, price: Double)
  case class UserInfo(id: Long, isMember: Boolean)

  sealed trait RecsError
  case class  InvalidUser(user: User) extends RecsError
  case object NoRecs                  extends RecsError

  def getTopRecommend(recs: List[Product]): Either[RecsError, Product] =
    recs.sortBy(_.id).headOption match {
      case Some(value) => scala.Right(value)
      case None =>        scala.Left(NoRecs)
    }

  val logError: RecsError => Unit = {
    case InvalidUser(u) => println(s"invalid user: ${u.id}")
    case NoRecs => println("noRecs")
  }


  // Smart constructors
  val getUserInfo         = liftK(GetUserInfo)
  val getProductInfo      = liftK(GetProductInfo)
  val getRecommendations  = liftK(GetRecommendations)
  val needsRecommendation = liftK(ValidateUser)
  val sendRecommend       = liftK(SendRecommend)

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

    val usersInfSource: Source[User, NotUsed] = Source.fromIterator(() => Stream.iterate(getUser)(_ => getUser).toIterator)

    val toAkka = new (Recs ~~> AkkaFlow) {

      def apply[A, B](fab: Recs[A, B]): AkkaFlow[A, B] = fab match {

        case GetRecommendations => Flow[User].map { u =>
          val r = new Random(u.id)
          val prods = List.fill(10)(products(math.abs(r.nextInt() % SEED)))
          prods
        }

        case GetUserInfo => Flow[User].map(u => userInfo(u.id))

        case GetProductInfo => Flow[Product].map(p => productInfo(p.id))

        case ValidateUser => Flow[UserInfo].map(_.isMember)

        case SendRecommend => Flow[Product]
      }
    }

    val pure = new ToFunction[Recs] {
      def apply[A, B](fab: Recs[A, B]): A => B = fab match {
        case GetUserInfo => (u: User) => userInfo(u.id)
        case GetRecommendations => (u: User) => {
          val r = new Random(u.id)
          val prods = List.fill(10)(products(math.abs(r.nextInt() % SEED)))
          prods
        }
        case GetProductInfo => (p: Product) => productInfo(p.id)
        case ValidateUser => ((_: UserInfo).isMember)
        case SendRecommend => identity[Product]
      }
    }
  }
}
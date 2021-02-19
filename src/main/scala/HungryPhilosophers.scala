import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

object HungryPhilosophers extends App {
  private val systemActors = ActorSystem("SystemActors")
  private val actorWaiter = systemActors.actorOf(Props[Waiter])

  private val listOfPhilosophers =
    for ((name, i) <- List("Aristotle", "Kant", "Platon", "Confucius", "Dekart")
      .zipWithIndex) yield systemActors.actorOf(Props(classOf[Philosopher], name, i, (i + 4) % 5, 0))

  listOfPhilosophers.map(x => x ! Start)

  systemActors.scheduler.scheduleOnce(25.seconds)(systemActors.terminate())

  case object Start

  case object Idea

  case object TheEnd

  case class TakeFork(fork: Int, numberForksInHand: Int)

  case class GetFork(fork: Int)

  case object forkNotFree

  case class PutFork(name: String, listForks: List[Int])

  case object TryTakeForks

  case object Full

  case object startEating

  class Waiter extends Actor {
    private val forks = mutable.Set(0, 1, 2, 3, 4)

    override def receive: Receive = {
      case TakeFork(fork, numberForksInHand) =>
        if (forks.contains(fork) && (forks.size != 1 || numberForksInHand == 1)) {
          forks.remove(fork)
          sender() ! GetFork(fork)
        } else {
          sender() ! forkNotFree
        }

      case PutFork(name, listForks) =>
        forks.addAll(listForks)
        println(s"$name положил ${listForks.head} и ${listForks(1)} вилку")
    }
  }

  class Philosopher(name: String, leftForkNumber: Int, rightForkNumber: Int, numberForksInHand: Int) extends Actor {

    override def receive: Receive = {
      case Start =>
        context.become(think)
        self ! Idea
    }

    def think: Receive = {
      case Idea =>
        println("Я " + name + " мои вилки " + leftForkNumber + " и " + rightForkNumber + "  и  я думаю")
        context.become(starving)
        println(name + " проголодался")
        context.system.scheduler.scheduleOnce(1.second)(self ! TryTakeForks)

      case TheEnd =>
        println("Философ " + name + " наелся и размышлят о вечном")
        self ! Idea
    }

    def starving: Receive = {
      case TryTakeForks => actorWaiter ! TakeFork(leftForkNumber, numberForksInHand)

      case GetFork(fork) =>
        context.become(waitRightFork)
        println(name + " взял левую вилку " + fork)
        context.system.scheduler.scheduleOnce(1.second)(actorWaiter ! TakeFork(rightForkNumber, numberForksInHand + 1))

      case forkNotFree =>
        context.system.scheduler.scheduleOnce(2.second)(actorWaiter ! TakeFork(leftForkNumber, numberForksInHand))
    }

    def waitRightFork: Receive = {
      case GetFork(fork) =>
        println(name + " взял правую вилку " + fork)
        context.become(eating)
        context.system.scheduler.scheduleOnce(1.second)(self ! startEating)

      case forkNotFree =>
        context.system.scheduler.scheduleOnce(2.second)(actorWaiter ! TakeFork(rightForkNumber, numberForksInHand + 1))
    }

    def eating: Receive = {
      case startEating => println(name + " кушает")
        context.become(satiety)
        context.system.scheduler.scheduleOnce(1.second)(self ! Full)
    }

    def satiety: Receive = {
      case Full =>
        println(name + " наелся")
        actorWaiter ! PutFork(name, List(leftForkNumber, rightForkNumber))
        context.become(think)
        context.system.scheduler.scheduleOnce(1.seconds)(self ! TheEnd)
    }
  }

}

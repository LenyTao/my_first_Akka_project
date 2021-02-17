import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

object HungryPhilosophers extends App {
  private val systemActors = ActorSystem("SystemActors")
  private val actorWaiter = systemActors.actorOf(Props[Waiter])

  private val listOfPhilosophers =
    for ((name, i) <- List("Aristotle", "Kant", "Platon", "Confucius", "Dekart")
      .zipWithIndex) yield systemActors.actorOf(Props(classOf[Philosopher], name, i, (i + 1) % 5))

  listOfPhilosophers.map(x => x ! Start)

  systemActors.scheduler.scheduleOnce(25.seconds)(systemActors.terminate())

  case class Start()

  case class Idea()

  case class TheEnd()

  case class TakeLeftFork(leftFork: Int)

  case class TakeRightFork(name: String, rightFork: Int, leftFork: Int)

  case class GetFirstFork(numberOfFork: Int)

  case class GetSecondFork(numberOfFork: Int)

  case class forkIsBusy()

  case class PutFork(name: String, numberOfFork: Int)

  case class TryTakeForks()

  case class Full()

  case class startEat()

  class Waiter extends Actor {
    private val forks = ListBuffer(0, 1, 2, 3, 4)

    override def receive: Receive = {
      case TakeLeftFork(numberLeftFork) =>
        if (forks.contains(numberLeftFork) && forks.length >= 2) {
          forks.remove(forks.indexOf(numberLeftFork))
          sender() ! GetFirstFork(numberLeftFork)
        } else {
          sender() ! forkIsBusy()
        }

      case TakeRightFork(name, numberRightFork, numberLeftFork) =>
        if (forks.contains(numberRightFork)) {
          forks.remove(forks.indexOf(numberRightFork))
          sender() ! GetSecondFork(numberRightFork)
        } else {
          self ! PutFork(name, numberLeftFork)
          sender() ! forkIsBusy()
        }

      case PutFork(name: String, numberFork: Int) =>
        forks.append(numberFork)
        println(name + " положил вилку " + numberFork)
    }
  }

  class Philosopher(name: String, leftForkNumber: Int, rightForkNumber: Int) extends Actor {
    var satietyPhilosopher: Boolean = false

    override def receive: Receive = {
      case Start =>
        context.become(think)
        self ! Idea
    }

    def think: Receive = {
      case Idea =>
        println("Я " + name + " мои вилки " + leftForkNumber + " и " + rightForkNumber + "  и  я думаю")
        if (!satietyPhilosopher) {
          context.become(starving)
          Thread.sleep(2000)
          println(name + " проголодался")
          context.system.scheduler.scheduleOnce(1.second)(self ! TryTakeForks)
        }
        else {
          self ! TheEnd
        }

      case TheEnd =>
        println("Философ " + name + " наелся и размышлят о вечном ожидая остальных")
    }

    def starving: Receive = {
      case TryTakeForks => actorWaiter ! TakeLeftFork(leftForkNumber)

      case GetFirstFork(numberOfFork) =>
        context.become(waitRightFork)
        println(name + " взял левую вилку " + numberOfFork)
        actorWaiter ! TakeRightFork(name, rightForkNumber, leftForkNumber)

      case forkIsBusy() =>
        context.system.scheduler.scheduleOnce(2.second)(self ! TryTakeForks)
    }

    def waitRightFork: Receive = {
      case GetSecondFork(numberOfFork2) =>
        println(name + " взял правую вилку " + numberOfFork2)
        context.become(eating)
        context.system.scheduler.scheduleOnce(1.second)(self ! startEat())

      case forkIsBusy() => context.become(starving)
        context.system.scheduler.scheduleOnce(1.second)(self ! TryTakeForks)
    }

    def eating: Receive = {
      case startEat() => println(name + " кушает")
        context.become(satiety)
        Thread.sleep(3000)
        context.system.scheduler.scheduleOnce(1.second)(self ! Full)
    }

    def satiety: Receive = {
      case Full =>
        println(name + " наелся")
        satietyPhilosopher = true
        actorWaiter ! PutFork(name, leftForkNumber)
        actorWaiter ! PutFork(name, rightForkNumber)
        context.become(think)
        context.system.scheduler.scheduleOnce(2.seconds)(self ! TheEnd)
    }
  }
}

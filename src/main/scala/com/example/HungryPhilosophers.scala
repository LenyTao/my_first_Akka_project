package com.example

import akka.actor.{Actor, ActorSystem, Props}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

object HungryPhilosophers extends App {
  private val systemActors = ActorSystem("SystemActors")
  private val actorWaiter = systemActors.actorOf(Props[Waiter])
  private val forks = ListBuffer(0, 1, 2, 3, 4)
  private val listOfPhilosophers =
    for ((name, i) <- List("Aristotle", "Kant", "Platon", "Confucius", "Dekart")
      .zip(forks)) yield systemActors.actorOf(Props(classOf[Philosopher], name, i, (i + 1) % 5))
  listOfPhilosophers.map(x => x ! Think)

  systemActors.scheduler.scheduleOnce(20.seconds)(systemActors.terminate())

  case class Think()

  case class Hungry()

  case class TakeForks(leftFork: Int, rightFork: Int)

  case class GetFirstFork(numberOfFork: Int)

  case class GetSecondFork(numberOfFork: Int)

  case class WaitingForks(numberOfFork: Int, numberOfFork2: Int)

  case class PutForks(name: String, numberOfFork: Int, numberOfFork2: Int)

  case class Full()

  case class Eat()

  class Waiter extends Actor {
    override def receive: Receive = {
      case TakeForks(leftFork: Int, rightFork: Int) =>
        if (forks.contains(leftFork) && forks.contains(rightFork)) {
          forks.remove(forks.indexOf(leftFork))
          sender() ! GetFirstFork(leftFork)
          forks.remove(forks.indexOf(rightFork))
          sender() ! GetSecondFork(rightFork)
        } else {
          sender() ! WaitingForks(leftFork, rightFork)
        }

      case PutForks(name: String, leftFork: Int, rightFork: Int) =>
        forks.append(leftFork)
        println(name + " положил вилку " + leftFork)
        forks.append(rightFork)
        println(name + " положил вилку " + rightFork)
    }

  }

  class Philosopher(name: String, leftForkNumber: Int, rightForkNumber: Int) extends Actor {

    override def receive: Receive = {
      case Think =>
        println("Я " + name + " мои вилки " + leftForkNumber + " и " + rightForkNumber + "  и  я думаю")
        context.become(starving)
        context.system.scheduler.scheduleOnce(5.second)(self ! Hungry)
    }

    def starving: Receive = {
      case Hungry =>
        println(name + " проголодался")
        actorWaiter ! TakeForks(leftForkNumber, rightForkNumber)
        context.become(waitingForks)
    }

    def waitingForks: Receive = {
      case GetFirstFork(numberOfFork) =>
        println(name + " взял вилку " + numberOfFork)
      case GetSecondFork(numberOfFork2) =>
        println(name + " взял вилку " + numberOfFork2)
        context.become(eating)
        self ! Eat

      case WaitingForks(numberOfFork, numberOfFork2) =>
        println(name + " Ждёт чтобы обе вилки с номерами: " + numberOfFork + " и " + numberOfFork2 + " были свободны")
        context.system.scheduler.scheduleOnce(2.seconds)(actorWaiter ! TakeForks(leftForkNumber, rightForkNumber))
    }

    def eating: Receive = {
      case Eat => println(name + " кушает")
        context.become(satiety)
        context.system.scheduler.scheduleOnce(3.seconds)(self ! Full)
    }

    def satiety: Receive = {
      case Full => actorWaiter ! PutForks(name, leftForkNumber, rightForkNumber)
        println(name + " наелся")
    }
  }

}

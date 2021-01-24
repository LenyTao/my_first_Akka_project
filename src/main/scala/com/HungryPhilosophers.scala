package com

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorLvl3 extends App {
  var forksOnTable = 5
  val philosopherAtTheTable = List(
    new Philosopher("Aristotle"),
    new Philosopher("Kant"),
    new Philosopher("Platon"),
    new Philosopher("Confucius"),
    new Philosopher("Dekart")

  )

  val systemActors = ActorSystem("SystemActors")
  val actorWaiter = systemActors.actorOf(Props[Waiter])

  println("Здравствуйте, коллеги садитесь за стол и начнём же обсуждения проблем мироздания!")
  println()

  philosopherAtTheTable.map(x => actorWaiter ! AskWaiter(x, actorWaiter))

  case class AskWaiter(philosopher: Philosopher, refOnThisActor: ActorRef)

  case class TakeLeftFork(philosopher: Philosopher, refOnThisActor: ActorRef)

  case class TakeRightFork(philosopher: Philosopher)

  case class PutLeftFork(philosopher: Philosopher)

  case class PutRightFork(philosopher: Philosopher)

  case class StartEating(philosopher: Philosopher)

  case class EndEating(philosopher: Philosopher)

  case class TheEnd()

  class Waiter extends Actor {
    override def receive: Receive = {
      case AskWaiter(philosopher, refOnThisActor) =>
        if (forksOnTable > 2) {
          forksOnTable -= 1
          println(philosopher.getName() + " Получил разрешение взять вилку")
          systemActors.actorOf(Props[Etiquette]) ! TakeLeftFork(philosopher, context.actorOf(Props[Etiquette]))
        } else {
          refOnThisActor ! AskWaiter(philosopher, refOnThisActor)
        }
    }
  }

  class Etiquette extends Actor {
    override def receive: Receive = {
      case TakeLeftFork(philosopher: Philosopher, refOnThisActor: ActorRef) =>
        if (forksOnTable > 0) {
          forksOnTable -= 1
          println()
          philosopher.takeLeftFork()
          println(philosopher.getName() + " Взял левую вилку")
          refOnThisActor ! TakeRightFork(philosopher)
        }
        else {
          refOnThisActor ! TakeLeftFork(philosopher, refOnThisActor)
        }
      case TakeRightFork(philosopher: Philosopher) =>
        println()
        philosopher.takeRightFork()
        println(philosopher.getName() + " Взял правую вилку")
        systemActors.actorOf(Props[Dinner]) ! StartEating(philosopher)

      case PutLeftFork(philosopher: Philosopher) =>
        println()
        philosopher.putLeftFork()
        println(philosopher.getName() + " Положил левую вилку")
        forksOnTable += 1
      case PutRightFork(philosopher: Philosopher) =>
        println()
        philosopher.putRightFork()
        println(philosopher.getName() + " Положил правую вилку")
        forksOnTable += 1
        if (philosopher.getMeals() == 5) {
          philosopher.makeFull()
          println(philosopher.getName() + " наелся")
          sender() ! TheEnd
        }
        else {
          actorWaiter ! AskWaiter(philosopher, actorWaiter)
        }
    }
  }

  class Dinner extends Actor {
    override def receive: Receive = {
      case StartEating(philosopher: Philosopher) =>
        if (philosopher.getNumberOfFork() == 2) {
          println()
          println(philosopher.getName() + " Начал трапезу")
          self ! EndEating(philosopher)
        } else {
          self ! StartEating(philosopher)
        }

      case EndEating(philosopher: Philosopher) =>
        philosopher.addOneMeals()
        println()
        println(philosopher.getName() + " Закончил трапезу")
        systemActors.actorOf(Props[Etiquette]) ! PutLeftFork(philosopher)
        systemActors.actorOf(Props[Etiquette]) ! PutRightFork(philosopher)

      case TheEnd =>
        if (philosopherAtTheTable.forall(x => x.getSatiety())) {
          println()
          println("Отличная посидели ребята, всё было очень вкусно, всем спасибо, до свидания!")
          context.system.terminate()
        }
    }
  }

}

class Philosopher(name: String) {
  private var satiety = false
  private var numberOfMeals = 0
  private var numberOfFork = 0

  def getNumberOfFork() = {
    numberOfFork
  }

  def takeLeftFork() = {
    numberOfFork += 1
  }

  def putLeftFork() = {
    numberOfFork -= 1
  }

  def takeRightFork() = {
    numberOfFork += 1
  }

  def putRightFork() = {
    numberOfFork -= 1
  }

  def getName(): String = {
    name
  }

  def makeFull(): Unit = {
    satiety = true
  }

  def getSatiety(): Boolean = {
    satiety
  }

  def addOneMeals(): Unit = {
    numberOfMeals += 1
  }

  def getMeals() = {
    numberOfMeals
  }
}
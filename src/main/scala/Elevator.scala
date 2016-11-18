import Elevator._
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.concurrent.duration._

/**
  * Created by inakov on 17.11.16.
  */

object Elevator {
  case class AddStop(floor: Int)
  case object CloseDoors
  case class Arrived(floor: Int)
  case object NextFloor

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object None extends Direction

  object MinOrder extends Ordering[Int] {
    def compare(x:Int, y:Int) = y compare x
  }

  def props(capacity: Int = 4) = Props(new Elevator(capacity))
}

class Elevator(val capacity: Int) extends Actor{
  import context._
  val log = Logging(system, this)

  var currentFloor: Int = 1
  val stops = collection.mutable.PriorityQueue.empty(MinOrder)

  override def receive: Receive = stoppedState

  def stoppedState = stopped orElse baseBehavior
  def movingUpState = movingUp orElse baseBehavior
  def movingDownState = movingDown orElse baseBehavior

  private def stopped: Receive = {
    case AddStop(floor) if floor != currentFloor =>
      stops.enqueue(floor)
      log.info(s"Added floor: $floor to elevator stops.")
    case CloseDoors if stops.nonEmpty =>
      moveToNextFloor
      if(stops.head - currentFloor < 0){
        become(movingDownState)
        parent ! Down
      }else {
        become(movingUpState)
        parent ! Up
      }
  }

  private def movingUp: Receive = {
    case AddStop(floor) if floor > currentFloor =>
      stops.enqueue(floor)
      log.info(s"Added floor: $floor to elevator stops.")
    case NextFloor =>
      currentFloor += 1
      checkForNextStop
  }

  private def movingDown: Receive = {
    case AddStop(floor) if floor < currentFloor =>
      stops.enqueue(floor)
      log.info(s"Added floor: $floor to elevator stops.")
    case NextFloor =>
      currentFloor -= 1
      checkForNextStop
  }

  private def checkForNextStop = {
    if(stops.head == currentFloor) openDoors
    else moveToNextFloor
  }

  private def openDoors = {
    log.info(s"Open the doors at $currentFloor and become stopped.")
    val currentStop = stops.dequeue()
    parent ! Arrived(currentStop)
    become(stoppedState)
    if(stops.isEmpty) parent ! None
  }

  private def moveToNextFloor: Unit = {
    log.info("Move to the next floor.")
    system.scheduler.scheduleOnce(100.millis, self, NextFloor)
  }

  def baseBehavior: Receive = {
    case msg @ _ => log.error(s"Received unexpected $msg. Skipping the message")
  }

}

import Employee._
import akka.actor.{Actor, ActorRef, Props}
import akka.actor.Actor.Receive

/**
  * Created by inakov on 17.11.16.
  */
object Employee{
  case class TravelUp(from: Int)
  case class TravelDown(from: Int)

  def props(workFloor: Int = 5, dispatcherRef: ActorRef) = Props(new Employee(workFloor, dispatcherRef))
}

class Employee(workFloor: Int, dispatcherRef: ActorRef) extends Actor{

  private var currentFloor = 1
  private var targetFloor = workFloor

  override def receive: Receive = {

  }

  def queueForElevator() = {
    val direction = targetFloor - currentFloor < 0
    if(direction) dispatcherRef ! TravelDown(currentFloor)
    else dispatcherRef ! TravelUp(currentFloor)
  }

}

import Elevator.{AddStop, CloseDoors}
import akka.actor.ActorSystem

/**
  * Created by inakov on 17.11.16.
  */
object Office extends App{

  val system = ActorSystem("office")

  val elevator = system.actorOf(Elevator.props(), "elevator")

  elevator ! AddStop(5)
  elevator ! CloseDoors
  elevator ! AddStop(4)
  elevator ! AddStop(1)

}

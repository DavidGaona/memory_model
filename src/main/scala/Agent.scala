import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}


// Messages
case object RequestBelief
case class SendBelief(belief: Double, senderAgent: ActorRef)

case class AddToNeighborhood(agent: ActorRef)
case class SendAgentData(round: Int, name: String, belief: Double, isSpeaking: Boolean)
case class BeliefUpdated(belief: Double)
case class SendNeighbors(network: Vector[ActorRef], influences: Vector[Double]) // Agent -> NetworkSaver

// Actor
class Agent(agentDataSaver: ActorRef, networkSaver: ActorRef) extends Actor {
  import context.dispatcher

  var belief: Double = -1
  var prevBelief: Double = belief
  var publicBelief: Double = -1
  var tolRadius: Double = 0.4
  var neighbors: Vector[ActorRef] = Vector.empty
  var influences: Vector[Double] = Vector.empty
  var speaking: Boolean = true
  var prevSpeaking: Boolean = true
  var round = 0
  var selfInfluence = 1.0

  implicit val timeout: Timeout = Timeout(600.seconds) // Set a timeout for the ask pattern

  def receive: Receive = {
    case AddToNeighborhood(neighbor) =>
      neighbors = neighbors :+ neighbor

    case setNeighborInfluence(neighbor, influence) =>
      neighbors = neighbors :+ neighbor 
      influences = influences :+ influence
      selfInfluence -= influence

    case setInitialState(initialBelief) =>
      belief = initialBelief
      prevBelief = belief
      publicBelief = belief

    case RequestBelief =>
      sender() ! SendBelief(publicBelief, self)

    case UpdateBelief =>
      prevBelief = belief
      prevSpeaking = speaking
      if (prevSpeaking)
        publicBelief = prevBelief

      if (round == 0) {
        if (influences.isEmpty) generateInfluences()
        else influences = influences :+ selfInfluence
        snapshotAgentState()
        networkSaver ! SendNeighbors(neighbors, influences)
        //println(self.path.name + " neighbors: " + influences.mkString(", "))
      }
      round += 1

      fetchBeliefsFromNeighbors { beliefs =>
        var inFavor = 0
        var against = 0
        belief = 0
        beliefs.foreach {
          case SendBelief(neighborBelief, neighbor)  =>
            if (tolRadius >= math.abs(prevBelief - neighborBelief)) inFavor += 1
            else against += 1
            belief += neighborBelief * influences(neighbors.indexOf(neighbor))
        }
        belief += prevBelief * influences.last
        speaking = inFavor >= against
        snapshotAgentState()
        context.parent ! BeliefUpdated(belief)
      }

  }

  override def preStart(): Unit = {
    val bimodal = new BimodalDistribution(0.25, 0.75)
    belief = bimodal.sample()
    prevBelief = belief
    publicBelief = belief
  }

  def fetchBeliefsFromNeighbors(callback: Seq[SendBelief] => Unit): Unit = {
    val futures = neighbors.map(neighbor => (neighbor ? RequestBelief).mapTo[SendBelief])
    val aggregatedFutures = Future.sequence(futures)

    aggregatedFutures.onComplete {
      case Success(beliefs) =>
        callback(beliefs)

      case Failure(exception) =>
        println(s"Error retrieving beliefs from neighbors: $exception")
    }
  }

  def generateInfluences(): Unit = {
    val random = new Random
    val randomNumbers = Vector.fill(neighbors.size + 1)(random.nextDouble())
    val sum = randomNumbers.sum
    influences = randomNumbers.map(_ / sum)
  }

  def snapshotAgentState(): Unit = {
    agentDataSaver ! SendAgentData(round, self.path.name, belief, speaking)
  }
}

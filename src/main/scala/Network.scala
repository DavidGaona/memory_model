import akka.actor.{Actor, ActorRef, ActorSelection, Props}
import jdk.internal.agent.resources.agent

case class setInitialState(belief: Double)

case class setNeighborInfluence(neighbor: ActorRef, influence: Double)

case object BuildingComplete

case object RunningComplete

case object UpdateBelief

case object SaveAgentsState

class Network(numberOfAgents: Int, density: Int, degreeDistributionParameter: Double, dataSavingPath: String)
    extends Actor {
  val agents: Array[ActorRef] = Array.ofDim[ActorRef](numberOfAgents)
  val maxIterations: Int = 10000
  val monitor: ActorRef = context.parent
  val agentDataSaver: ActorRef = context.actorOf(Props(new AgentDataSaver(dataSavingPath)))
  val networkSaver: ActorRef = context.actorOf(Props(new NetworkSaver(dataSavingPath)))
  var pendingResponses = 0
  var round = 0
  var minBelief = 2.0
  var maxBelief: Double = -1.0
  var stopThreshold = 0.001

  def receive: Receive = building

  def createNewAgent(agentName: String): ActorRef = {
    context.actorOf(Props(new Agent(agentDataSaver, networkSaver)), agentName)
  }

  def building: Receive = {
    case BuildNetwork =>
      val fenwickTree = new FenwickTree(numberOfAgents, density, degreeDistributionParameter - 2)

      // Initialize the first n=density agents
      for (i <- 0 until density) {
        val newAgent = createNewAgent(s"Agent${i + 1}")
        agents(i) = newAgent
        for (j <- 0 until i) {
          agents(j) ! AddToNeighborhood(newAgent)
          newAgent ! AddToNeighborhood(agents(j))
        }
      }

      // Create and link the agents
      for (i <- density - 1 until numberOfAgents - 1) {
        // Create the new agent
        val newAgent = createNewAgent(s"Agent${i + 2}")
        agents(i + 1) = newAgent

        // Pick the agents based on their attractiveness score and link them
        val agentsPicked = fenwickTree.pickRandoms()
        agentsPicked.foreach { agent =>
          agents(agent) ! AddToNeighborhood(newAgent)
          newAgent ! AddToNeighborhood(agents(agent))
        }
      }

      context.become(running)
      monitor ! BuildingComplete

    case BuildCustomNetwork(agents) =>
      for (i <- agents.indices){
        val newAgent = createNewAgent(agents(i).name)
        this.agents(i) = newAgent
        agents(i).neighbors.foreach((agentName, influence) =>
          this.agents.filter(_ != null).find(agent => agent.path.name == agentName) match {
            case Some(neighbor) =>
              newAgent ! setNeighborInfluence(neighbor, influence)
              val influenceN = agents.flatMap(_.neighbors)
                  .find(_._1 == newAgent.path.name)
                  .map(_._2)
                  .get
              neighbor ! setNeighborInfluence(newAgent, influenceN)
            case None =>
          }
        )
        newAgent ! setInitialState(agents(i).initialBelief)
      }

      context.become(running)
      monitor ! BuildingComplete
  }


  def running: Receive = {
    case RunNetwork =>
      pendingResponses = agents.length
      agents.foreach { agent => agent ! UpdateBelief }

    case BeliefUpdated(belief) =>
      pendingResponses -= 1
      minBelief = math.min(belief, minBelief)
      maxBelief = math.max(belief, maxBelief)
      if (pendingResponses == 0) {
        if (round == 0) networkSaver ! ExportNetwork(self.path.name)
        round += 1
        //println(s"Round: $round, Min: $minBelief, Max: $maxBelief, dif: ${(maxBelief - minBelief)}")
        if (round >= maxIterations || (maxBelief - minBelief) < stopThreshold)
          monitor ! RunningComplete
          context.become(analyzing)
          if ((maxBelief - minBelief) < stopThreshold) {
            //println(s"Reached consensus ${self.path.name}")
          } else println(s"Did no reach consensus ${self.path.name}")
        else
          self ! RunNetwork
          minBelief = 2.0
          maxBelief = -1.0
      }
  }

  def analyzing: Receive = {
    case StartAnalysis =>
      agentDataSaver ! SaveAgentsState
  }
}

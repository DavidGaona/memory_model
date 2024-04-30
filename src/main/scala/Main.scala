import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case class AddNetworks(numberOfNetworks: Int, numberOfAgents: Int, density: Int, degreeDistribution: Double)
case class AddSpecificNetwork(agents: Array[AgentInitialState], name: String)
case class AgentInitialState
(
  name: String,
  initialBelief: Double,
  neighbors: Array[(String, Double)]
)

object Main extends App {
  val system = ActorSystem("simplified")
  val monitor = system.actorOf(Props(new Monitor()), "Monitor")
  monitor ! AddNetworks(50000, 4, 1, 2.5)
  val network: Array[AgentInitialState] = Array(
    // Agent name, initial belief, neighbors list, influence list

    AgentInitialState("Agent1", 0.963607, Array(("Agent2", 0.5693133738025712), ("Agent4", 0.23599470465272282))),
    AgentInitialState("Agent2", 0.642179, Array(("Agent1", 0.6391417559510141), ("Agent3", 0.14901345076964342))),
    AgentInitialState("Agent3", 0.850725, Array(("Agent2", 0.6446363101864195))),
    AgentInitialState("Agent4", 0.365930, Array(("Agent1", 0.6678693224077077)))


    // AgentInitialState("Agent1", 0.5, Array(("Agent2", 0.9), ("Agent3", 0.03), ("Agent4", 0.02))),
    //    AgentInitialState("Agent2", 0.4, Array(("Agent1", 0.9))),
    //    AgentInitialState("Agent3", 0.9, Array(("Agent1", 0.05))),
    //    AgentInitialState("Agent4", 0.9, Array(("Agent1", 0.05)))

    // Smallest disagreement possible
    // AgentInitialState("Agent1", 0.5, Array(("Agent2", 0.4), ("Agent3", 0.4))),
    //    AgentInitialState("Agent2", 0.0, Array(("Agent4", 0.95), ("Agent1", 0.01))),
    //    AgentInitialState("Agent3", 1.0, Array(("Agent5", 0.95), ("Agent1", 0.01))),
    //    AgentInitialState("Agent4", 0.0, Array(("Agent2", 0.05))),
    //    AgentInitialState("Agent5", 1.0, Array(("Agent3", 0.05)))
  )

  //monitor ! AddSpecificNetwork(network, "try")
}

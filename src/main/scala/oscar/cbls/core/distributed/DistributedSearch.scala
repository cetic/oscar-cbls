package oscar.cbls.core.distributed

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, ActorSystem}
import oscar.cbls.Neighborhood
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.actors._
import oscar.cbls.core.distributed.computation.SearchConnector
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.TestBehavior

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** This is the entry point for using distributed search.
  */
object DistributedSearch {

  /**
   * A remote worker node that spawns workers connecting to a remote supervisor.
   * The node receives problem data (ProblemStatement) from the supervisor and uses
   * ProblemStatement.buildLocalSearchModel() to create models.
   */
  class RemoteWorkerNode(
                          config: Config,
                          nbWorkers: Int,
                          verbose: Int,
                          supervisorHost: String,
                          supervisorPort: Int
                        ) {

    // Create actor system for this worker node
    private val workerSystem: ActorSystem[MessageToWorkerNode] = ActorSystem(
      WorkerNode(nbWorkers, verbose, supervisorHost, supervisorPort),
      "Supervisor",
      config
    )

    /**
     * Blocks until the worker node shuts down.
     */
    def awaitTermination(): Unit = {
      Await.result(workerSystem.whenTerminated, Duration.Inf)
    }

    /**
     * Initiates shutdown of this worker node.
     */
    def shutdown(): Unit = {
      workerSystem.terminate()
    }
  }

  /** a helper that return the number of cores of your system so that you have an idea of how many
    * workers you can run. Beware however that these might be logical cores; not physical ones and
    * that there might be other limitations such as the number of lanes between the CPU and the RAM,
    * sop that the optimal number of workers might be smaller than the number of cores returned by
    * this method.
    * @return
    *   the number of cores of your system [[Runtime.getRuntime.availableProcessors]]
    */
  def nbCores: Int = Runtime.getRuntime.availableProcessors()

  /** Starts up the distributed search system; something that will distribute the work across
    * workers. It does not create the workers per se, you have to call
    * [[DistributedSearch.spawnLocalWorker]] to create the workers
    *
    * @param store
    *   the store of the main search procedure
    * @param verbose
    *   verbosity for the distributed part
    *   - 0: no messages
    *   - 1: joining and leaving workers
    *   - 2: every worker operation
    * @param search
    *   the main instance of search procedures that you will run using doAllMoves (there can be
    *   several search procedures; if you want to do several doAllMoves; however typical
    *   applications will only have one)
    * @return
    *   a [[DistributedSearch]] from which you can control the distributed search machinery (besides
    *   running, the search, which is done through [[Neighborhood.doAllMoves]] as usual) through
    *   this object, you can
    *   - start new local workers, typically one per physical core of your CPU
    *   - shut down the system
    */
  def apply(store: Store, verbose: Int, search: Neighborhood*): DistributedSearch = {
    apply(store, verbose, distributed = false, search: _*)
  }

  /**
   * Creates a distributed search with option for multi-JVM support.
   *
   * @param store The store of the main search procedure
   * @param verbose Verbosity level (0: none, 1: join/leave, 2: all operations)
   * @param distributed If true, enables cluster mode for remote workers
   * @param search The neighborhoods to explore
   */
  def apply(
             store: Store,
             verbose: Int,
             distributed: Boolean,
             search: Neighborhood*
           ): DistributedSearch = {
    apply(store, verbose, distributed, None, search: _*)
  }

  /**
   * Creates a distributed search with option for multi-JVM support and problem data.
   *
   * @param store The store of the main search procedure
   * @param verbose Verbosity level (0: none, 1: join/leave, 2: all operations)
   * @param distributed If true, enables cluster mode for remote workers
   * @param problemStatement Optional problem statement to send to remote worker nodes.
   *                         Workers will call buildLocalSearchModel() to create their models.
   * @param search The neighborhoods to explore
   */
  def apply(
             store: Store,
             verbose: Int,
             distributed: Boolean,
             problemStatement: Option[ProblemStatement],
             search: Neighborhood*
           ): DistributedSearch = {
    val config = if (distributed) {
      loadDistributedConfig()
    } else {
      loadLocalConfig()
    }
    createWithConfig(store, verbose, config, distributed, problemStatement, search: _*)
  }

  /**
   * Creates a distributed search supervisor with custom host and port.
   *
   * @param store The store of the main search procedure
   * @param verbose Verbosity level (0: none, 1: join/leave, 2: all operations)
   * @param distributed If true, enables cluster mode for remote workers
   * @param problemStatement Optional problem statement to send to remote worker nodes.
   *                         Workers will call buildLocalSearchModel() to create their models.
   * @param supervisorHost Hostname for the supervisor (e.g., "127.0.0.1" or machine IP)
   * @param supervisorPort Port for the supervisor (e.g., 2551)
   * @param search The neighborhoods to explore
   */
  def apply(
             store: Store,
             verbose: Int,
             distributed: Boolean,
             problemStatement: Option[ProblemStatement],
             supervisorHost: String,
             supervisorPort: Int,
             search: Neighborhood*
           ): DistributedSearch = {
    val config = if (distributed) {
      loadDistributedConfig(supervisorHost, supervisorPort)
    } else {
      loadLocalConfig()
    }
    createWithConfig(store, verbose, config, distributed, problemStatement, search: _*)
  }

  /**
   * Starts a remote worker node that connects to a supervisor.
   * The node will receive problem data (ProblemStatement) from the supervisor and use
   * ProblemStatement.buildLocalSearchModel() to create models.
   *
   * @param supervisorHost Hostname of the supervisor
   * @param supervisorPort Port of the supervisor
   * @param workerHost Hostname for this worker (use local IP for remote deployment)
   * @param workerPort Port for this worker (0 for auto-assign)
   * @param nbWorkers Number of workers to spawn in this JVM
   * @param verbose Verbosity level
   */
  def apply(
             supervisorHost: String,
             supervisorPort: Int,
             workerHost: String = "127.0.0.1",
             workerPort: Int = 0,
             nbWorkers: Int = 1,
             verbose: Int = 1
           ): RemoteWorkerNode = {

    val config = ConfigFactory.parseString(
      s"""
pekko {
  # Logging configuration
  loglevel = "INFO"
  log-dead-letters = 0
  log-dead-letters-during-shutdown = false
  actor {
    # Enable cluster provider for distributed mode
    # Override to "local" for mono-JVM mode
    provider = "cluster"
	allow-java-serialization = off
    warn-about-java-serializer-usage = on
    # Serialization configuration
    serializers {
      kryo = "io.altoo.serialization.kryo.pekko.PekkoKryoSerializer"
    }
    serialization-bindings {
      # Bind all protocol messages to Kryo serializer
      "oscar.cbls.core.distributed.protocol.MessageToWorker" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToSupervisor" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToSearch" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToWorkerNode" = kryo
      "oscar.cbls.core.distributed.computation.Task" = kryo
      "oscar.cbls.core.distributed.computation.TaskResult" = kryo
      "oscar.cbls.core.distributed.computation.TaskParameters" = kryo
      "oscar.cbls.core.distributed.computation.ActualResult" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentMove" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentSolution" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentSavedValue" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentObjective" = kryo
    }
    # Disable Java serialization for security and performance
    allow-java-serialization = off
    warn-about-java-serializer-usage = on
  }
  # Remote configuration using Artery (default modern transport)
  remote {
    artery {
      enabled = on
      transport = tcp
      canonical {
        # Default to localhost
        hostname = "$workerHost"
        port = $workerPort
      }
      # Advanced settings for handling large messages (solutions, moves)
      advanced {
        # Maximum size for regular messages
        maximum-frame-size = 4 MiB
        buffer-pool-size = 128
        # Maximum size for large messages (solutions with many variables)
        maximum-large-frame-size = 8 MiB
        large-buffer-pool-size = 32
        # Outbound message queue
        outbound-message-queue-size = 3072
        # Compression (optional - can reduce network traffic for large solutions)
        # compression {
        #   actor-refs.advertisement-interval = 1 minute
        #   manifests.advertisement-interval = 1 minute
        # }
      }
    }
    # Watch failure detector settings
    watch-failure-detector {
      heartbeat-interval = 1s
      threshold = 10.0
      acceptable-heartbeat-pause = 10s
    }
  }
  # Cluster configuration
  cluster {
    roles = ["worker"]
    # Seed nodes - the initial contact points for cluster formation
    # Override in deployment-specific configs
    seed-nodes = [
      "pekko://Supervisor@$supervisorHost:$supervisorPort"
    ]
    # Downing provider - handles network partitions
    downing-provider-class = "org.apache.pekko.cluster.sbr.SplitBrainResolverProvider"
    split-brain-resolver {
      # Keep the partition with majority of nodes
      active-strategy = keep-majority
      # Wait for stability before making decisions
      stable-after = 20s
      # Down unreachable nodes after this duration
      down-all-when-unstable = 15s
    }
    # Failure detection tuning
    failure-detector {
      # How often to send heartbeats
      heartbeat-interval = 1s
      # How long to wait without heartbeat before suspicion
      acceptable-heartbeat-pause = 3s
      # Phi accrual failure detector threshold
      threshold = 8.0
      # Minimum standard deviation to use for calculation
      min-std-deviation = 100ms
      # Number of samples for calculating mean and deviation
      max-sample-size = 1000
    }
    # Sharding settings (for future use if needed)
    # sharding {
    #   state-store-mode = ddata
    # }
  }
}
##############################################
# Pekko Kryo Serialization Configuration
##############################################
pekko-kryo-serialization {
  # Use custom initializer that registers all Oscar-CBLS protocol classes
  kryo-initializer = "oscar.cbls.core.distributed.serialization.OscarCblsKryoInitializer"
  # CRITICAL: Use Pekko's serialization for ActorRefs
  # This ensures ActorRefs are serialized as paths, not as object graphs
  resolve-subclasses = false
  # Buffer sizing
  buffer-size = 4096
  max-buffer-size = -1  # No limit
  # Use unsafe operations for better performance
  use-unsafe = true
  # Enable reference tracking to handle object graphs correctly
  kryo-reference-map = true
  # ID strategy:
  # - "default" writes full class names (slower but no registration issues)
  # - "incremental" requires explicit registration and consistent ordering
  # - "explicit" requires explicit registration with fixed IDs
  #
  # Using "default" to avoid class ID mismatch issues with ActorRef implementations
  # which are internal Pekko classes that cannot be explicitly registered.
  id-strategy = "default"
  # Log when implicit registration happens (helps debug missing registrations)
  implicit-registration-logging = true
  # Post serialization transforms (compression options)
  # Options: "off", "lz4", "deflate"
  post-serialization-transformations = "off"
  # CRITICAL: Use Pekko's ActorRef serialization
  # This delegates ActorRef serialization to Pekko's built-in serializers
  pekko {
    # Enable Pekko serialization for ActorRefs within Kryo
    actor-refs {
      # Use Pekko's serialization system for actor references
      serialize-creator = false
    }
  }
  # Classes to register with Kryo
  # These are registered in addition to the KryoInitializer
  classes = [
    "scala.collection.immutable.$$colon$$colon",
    "scala.collection.immutable.Nil$$",
    "scala.None$$",
    "scala.Some"
  ]
}
      """).withFallback(ConfigFactory.load())

    new RemoteWorkerNode(config, nbWorkers, verbose, supervisorHost, supervisorPort)
  }

  /**
   * Creates a distributed search supervisor with custom configuration.
   * Use this for advanced deployment scenarios.
   *
   * @param store The store of the main search procedure
   * @param verbose Verbosity level (0: none, 1: join/leave, 2: all operations)
   * @param config Custom Pekko configuration
   * @param problemStatement Optional problem statement to send to remote worker nodes.
   *                         Workers will call buildLocalSearchModel() to create their models.
   * @param search The neighborhoods to explore
   */
  def withConfig(
                  store: Store,
                  verbose: Int,
                  config: Config,
                  problemStatement: Option[ProblemStatement],
                  search: Neighborhood*
                ): DistributedSearch = {
    val isDistributed = config.getString("pekko.actor.provider") == "cluster"
    createWithConfig(store, verbose, config, isDistributed, problemStatement, search: _*)
  }

  private def loadLocalConfig(): Config = {
    ConfigFactory.parseString(
      s"""
        pekko {
          log-dead-letters = 0
          log-dead-letters-during-shutdown = false
          actor.provider = "local"
        }
      """
    ).withFallback(ConfigFactory.load())
  }

  private def loadDistributedConfig(): Config = {
    loadDistributedConfig("127.0.0.1", 2551)
  }

  private def loadDistributedConfig(supervisorHost: String, supervisorPort: Int): Config = {
    ConfigFactory.parseString(
      s"""
pekko {
  # Logging configuration
  loglevel = "INFO"
  log-dead-letters = 0
  log-dead-letters-during-shutdown = false
  actor {
    # Enable cluster provider for distributed mode
    provider = "cluster"
    allow-java-serialization = off
    warn-about-java-serializer-usage = on
    # Serialization configuration
    serializers {
      kryo = "io.altoo.serialization.kryo.pekko.PekkoKryoSerializer"
    }
    serialization-bindings {
      # Bind all protocol messages to Kryo serializer
      "oscar.cbls.core.distributed.protocol.MessageToWorker" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToSupervisor" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToSearch" = kryo
      "oscar.cbls.core.distributed.protocol.MessageToWorkerNode" = kryo
      "oscar.cbls.core.distributed.computation.Task" = kryo
      "oscar.cbls.core.distributed.computation.TaskResult" = kryo
      "oscar.cbls.core.distributed.computation.TaskParameters" = kryo
      "oscar.cbls.core.distributed.computation.ActualResult" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentMove" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentSolution" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentSavedValue" = kryo
      "oscar.cbls.core.distributed.computation.StoreIndependentObjective" = kryo
    }
  }
  remote {
    artery {
      enabled = on
      transport = tcp
      canonical {
        hostname = "$supervisorHost"
        port = $supervisorPort
      }
      advanced {
        maximum-frame-size = 4 MiB
        buffer-pool-size = 128
        maximum-large-frame-size = 8 MiB
        large-buffer-pool-size = 32
        outbound-message-queue-size = 3072
      }
    }
    watch-failure-detector {
      heartbeat-interval = 1s
      threshold = 10.0
      acceptable-heartbeat-pause = 10s
    }
  }
  cluster {
    roles = ["supervisor"]
    # Seed node with configurable host/port
    seed-nodes = [
      "pekko://Supervisor@$supervisorHost:$supervisorPort"
    ]
    downing-provider-class = "org.apache.pekko.cluster.sbr.SplitBrainResolverProvider"
    split-brain-resolver {
      active-strategy = keep-majority
      stable-after = 20s
      down-all-when-unstable = 15s
    }
    failure-detector {
      heartbeat-interval = 1s
      acceptable-heartbeat-pause = 3s
      threshold = 8.0
      min-std-deviation = 100ms
      max-sample-size = 1000
    }
  }
}
##############################################
# Pekko Kryo Serialization Configuration
##############################################
pekko-kryo-serialization {
  kryo-initializer = "oscar.cbls.core.distributed.serialization.OscarCblsKryoInitializer"
  resolve-subclasses = false
  buffer-size = 4096
  max-buffer-size = -1
  use-unsafe = true
  kryo-reference-map = true
  id-strategy = "default"
  implicit-registration-logging = true
  post-serialization-transformations = "off"
  pekko {
    actor-refs {
      serialize-creator = false
    }
  }
  classes = [
    "scala.collection.immutable.$$colon$$colon",
    "scala.collection.immutable.Nil$$",
    "scala.None$$",
    "scala.Some"
  ]
}
      """).withFallback(ConfigFactory.load())
  }

  private def createWithConfig(
                                store: Store,
                                verbose: Int,
                                config: Config,
                                distributed: Boolean,
                                problemStatement: Option[ProblemStatement],
                                search: Neighborhood*
                              ): DistributedSearch = {
    val searchConnector = new SearchConnector(store, search.toList)

    val refToSupervisor: ActorSystem[MessageToSupervisor] = ActorSystem(
      Behaviors.setup[MessageToSupervisor](_ =>
        Supervisor(searchConnector, verbose, problemStatement)
      ),
      "Supervisor",
      config
    )
    searchConnector.supervisor = refToSupervisor

    new DistributedSearch(
      refToSupervisor,
      refToSupervisor,
      searchConnector.checkSumOnStoreAndSearch,
      store,
      distributed
    )
  }

}

/** The class from which you can control the distributed search machinery (besides running, the
  * search, which is done through "Neighborhood.doAllMoves" as usual) through this object, you can
  *   - start new local workers, typically one per physical core of your CPU
  *   - shut down the system
  *
  * @param supervisor
  *   a reference to the supervisor actor
  * @param spawner
  *   an actor that is able to spawn local workers
  * @param checkSumOnStoreAndSearch
  *   a checksum on the store and search procedure, to check that workers and supervisors share hte
  *   same optimization model
  * @param supervisorStore
  *   the store allocated by the supervisor
  * @param isDistributed
  *   flag parameter to check whether the search is multi-JVM (true) or just multi-threading (false)
  */
class DistributedSearch(
                         supervisor: ActorSystem[MessageToSupervisor],
                         spawner: ActorRef[SpawnLocalWorker],
                         checkSumOnStoreAndSearch: Long,
                         supervisorStore: Store,
                         val isDistributed: Boolean = false
                       ) {

  private var storeList: List[Store] = supervisorStore :: Nil

  /**
   * Returns the supervisor's actor address for remote workers to connect.
   * Only meaningful when isDistributed is true.
   */
  def supervisorAddress: String = {
    if (isDistributed) {
      supervisor.address.toString
    } else {
      "local://" + supervisor.path
    }
  }

  /** call this to spawn a new local worker (at the same JVM as supervisor) that will operate on the
   * specified instances of store and search procedures. These must be different instance than the ones
   * of the supervisor however they must represent the exact same model
   *
   * @param store
   *   the Store
   * @param searches
   *   the list of neighborhoods that can be explored (see [[DistributedSearch.apply]])
   */
  def spawnLocalWorker(store: Store, searches: Neighborhood*): Unit = {
    spawnLocalWorkerWithTestBehavior(testBehavior = null, store = store, searches = searches: _*)
  }

  def spawnLocalWorkerWithTestBehavior(
                                        testBehavior: TestBehavior,
                                        store: Store,
                                        searches: Neighborhood*
                                      ): Unit = {
    val searchConnector = new SearchConnector(store, searches.toList)
    searchConnector.supervisor = supervisor
    require(
      searchConnector.checkSumOnStoreAndSearch == checkSumOnStoreAndSearch,
      "The store of search procedure of the worker are different from the main store and search procedures"
    )
    synchronized {
      // This is synchronized because since we are dealing with multi-threading,
      // users could decide to generate the workers in different threads,
      // so better be safe, just in case
      require(
        storeList.forall(_ ne store),
        "Workers cannot share the same instance of store or search procedure, neither with each other, nor with the supervisor"
      )
      storeList = store :: storeList
    }
    // Send message to spawner
    spawner ! SpawnLocalWorker(searchConnector, testBehaviorOpt = Option(testBehavior))
  }

  def status: String = supervisor.printTree

  /** Shut down the distributed search framework, telling all workers to shut down. Useless in case
    * of single-JVM operation if you exit the JVM anyway; but handy when the system operates on
    * multiple JVM.
    */
  def globalShutDown(): Unit = {
    supervisor ! GlobalShutDown
  }
}

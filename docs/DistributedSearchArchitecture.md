# Distributed Search Architecture

OscaR-CBLS provides a distributed search framework that enables parallel exploration of neighborhoods across multiple threads or multiple JVMs. This document explains the architecture and usage patterns.

## Overview

The distributed search framework is built on top of Apache Pekko actors and follows a **Supervisor-Worker** pattern. The main idea is to:

1. Split the search space into independent tasks
2. Distribute these tasks across multiple workers
3. Collect and aggregate results

This architecture supports two modes:
- **Single JVM (Multi-thread)**: All workers run in the same JVM, using local actor communication
- **Multi-JVM (Distributed)**: Workers can run on different machines, using network communication

## Architecture Components

### Core Components

```
+----------------+
|   Supervisor   |  - Coordinates all workers
|                |  - Distributes tasks
|                |  - Collects results
+-------+--------+
        |
        | (Messages)
        |
+-------v--------+     +----------------+     +----------------+
|    Worker 1    |     |    Worker 2    |     |    Worker N    |
|                |     |                |     |                |
| +------------+ |     | +------------+ |     | +------------+ |
| |   Store    | |     | |   Store    | |     | |   Store    | |
| +------------+ |     | +------------+ |     | +------------+ |
| |  Search    | |     | |  Search    | |     | |  Search    | |
| +------------+ |     | +------------+ |     | +------------+ |
+----------------+     +----------------+     +----------------+
```

### Key Classes

| Class | Package | Role |
|-------|---------|------|
| `DistributedSearch` | `oscar.cbls.core.distributed` | Entry point for setting up distributed search |
| `Supervisor` | `oscar.cbls.core.distributed.actors` | Actor that coordinates workers and distributes tasks |
| `Worker` | `oscar.cbls.core.distributed.actors` | Actor that executes tasks on its local Store |
| `WorkerNode` | `oscar.cbls.core.distributed.actors` | Guardian actor for remote workers in multi-JVM mode |
| `SearchConnector` | `oscar.cbls.core.distributed.computation` | Bridges between Store-dependent and Store-independent data |
| `Task` | `oscar.cbls.core.distributed.computation` | Represents a unit of work to be executed |
| `DistributedModulo` | `oscar.cbls.lib.neighborhoods.combinator.distributed` | Neighborhood combinator for parallel modulo-based search |
| `DistributedPopulationBased` | `oscar.cbls.lib.neighborhoods.combinator.distributed` | Population-based meta-heuristic with distributed diversification |

## How It Works

### Store Independence

A key challenge in distributed search is that each worker needs its **own copy** of the optimization model (Store). Solutions, moves, and objectives cannot be directly shared between stores.

The `SearchConnector` class provides methods to:
- **Detach** data from a Store (making it serializable)
- **Attach** data to a different Store (restoring it)

```scala
// Detach a solution from one store
val detached: StoreIndependentSolution = searchConnector.saveDetachedSolution

// Attach to another store
val solution: Solution = otherSearchConnector.attachSolutionToStore(detached)
```

### Task Execution Flow

1. **Search Combinator** creates tasks with `TaskParameters` (e.g., `GetModuloMove`)
2. **Supervisor** receives tasks and distributes them to available workers
3. **Worker** executes the task using its local Store and `RemotelyCallableTask`
4. **Worker** sends back `TaskResult` with the result
5. **Supervisor** forwards results to the Search Combinator
6. **Search Combinator** aggregates results and returns the best move

### Message Protocol

The framework uses typed actors with the following message hierarchies:

**MessageToSupervisor** (from Workers/Search):
- `WorkerRegister`: Worker announces itself
- `WorkerTaskFinished`: Task completed with result
- `CreateTask/CreateTasks`: Request to execute tasks
- `CancelTask/CancelAllMyRemainingTasks`: Cancel ongoing tasks

**MessageToWorker** (from Supervisor):
- `EnqueueTask`: Add task to worker's queue
- `KillTasks`: Cancel specific tasks
- `WorkerShutdown`: Terminate the worker

**MessageToSearch** (from Supervisor):
- `StatusReport`: Current worker availability
- `ResultObtained`: Task result
- `NewUniqueTaskIds`: Assigned task IDs

## Single JVM (Multi-thread) Mode

This mode is simpler to set up and suitable when you want to utilize multiple CPU cores on a single machine.

### Usage Pattern

```scala
import oscar.cbls._
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo.ModuloRange

// Function to create model and search (called multiple times)
def createModelAndSearch: (Store, Objective, Neighborhood) = {
  implicit val m: Model = model("My Problem")

  // Define variables, constraints, objective...
  val myVars = Array.tabulate(n)(i => intVar(domain, name = s"var_$i"))
  // Objective function in terms of myVars
  val obj = m.minimize(objectiveExpr)
  m.close()

  // Use DistributedModulo to split the search space
  val search = DistributedModulo((p: ModuloRange) =>
    Nrs.assign(
      myVars,
      searchZone = Some(() => p.offset until n by p.step),
      name = s"assignModulo(${p.step},${p.offset})"
    )
  )

  (m.store, obj, search)
}

// Create the supervisor
val (store, obj, search) = createModelAndSearch
val distributedSearch = DistributedSearch(store, verbose = 1, search = search)

// Spawn workers (one per CPU core, or fewer)
for (_ <- 0 until DistributedSearch.nbCores / 4) {
  val (workerStore, _, workerSearch) = createModelAndSearch
  distributedSearch.spawnLocalWorker(workerStore, workerSearch)
}

// Run the search
search.doAllMoves(obj)

// Shut down
distributedSearch.globalShutDown()
```

### Key Points

1. **Each worker needs its own Store**: Call `createModelAndSearch` for each worker
2. **Models must be identical**: The framework validates this using checksums
3. **Workers share no state**: All communication happens through messages
4. **Use a distributed combinator**: `DistributedModulo` splits a neighborhood for parallel exploration; `DistributedPopulationBased` evolves a population using parallel diversification

## Multi-JVM (Distributed) Mode

This mode allows workers to run on different machines, enabling distributed computing across a cluster.

### Architecture

```
+------------------+                    +------------------+
|  Supervisor JVM  |                    |  Worker Node 1   |
|                  |                    |                  |
|  +-----------+   |   Network (TCP)    |  +-----------+   |
|  | Supervisor|<------------------------->| WorkerNode|   |
|  +-----------+   |                    |  +-----------+   |
|  | Worker(s) |   |                    |  | Worker(s) |   |
|  +-----------+   |                    |  +-----------+   |
+------------------+                    +------------------+
                                                |
                                        +-------v----------+
                                        |  Worker Node 2   |
                                        |                  |
                                        |  +-----------+   |
                                        |  | WorkerNode|   |
                                        |  +-----------+   |
                                        |  | Worker(s) |   |
                                        |  +-----------+   |
                                        +------------------+
```

### Problem Statement

In multi-JVM mode, workers do not share code directly. Instead, the supervisor sends a **ProblemStatement** that workers use to build their local models:

```scala
case class MyProblemStatement(
  problemData: Array[Long],
  // ... other problem-specific data
) extends ProblemStatement {

  override def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood]) = {
    implicit val m: Model = model("My Problem")

    // Build the model using problemData
    val vars = Array.tabulate(problemData.length)(i =>
      intVar(domain, name = s"var_$i")
    )
    // Objective function in terms of myVars
    val obj = m.minimize(objectiveExpr)
    m.close()

    val search = DistributedModulo(/* ... */)

    (m.store, obj, Seq(search))
  }
}
```

#### `buildLocalSearchModel` must be deterministic

The statement is the *only* thing the nodes share. Each of them rebuilds its model on its own side,
and the solutions that then travel between them (`StoreIndependentSolution`) carry values only, not
the model these values were computed on: a solution is essentially a list of values, matched to
variables by their order in the store. So `buildLocalSearchModel` must be a **pure function of the
statement**: two calls with the same statement, in whatever JVM, must create the same variables, in
the same order, with the same initial values.

When it is not, nothing fails loudly. The nodes just quietly disagree on what a solution means, the
objective measured on a worker no longer matches the one measured on the supervisor, and the search
returns solutions that are wrong or worse than expected.

The usual sources of divergence are:

- **An unseeded `Random` used to build the initial solution.** Put the seed in the statement and
  derive the model from it, so that every node draws the same values:

  ```scala
  case class MyProblemStatement(
    problemData: Array[Long],
    initialSolutionSeed: Long   // travels with the statement: same seed on every node
  ) extends ProblemStatement {

    override def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood]) = {
      // Seeded from the statement: all the nodes build the same initial solution
      val initialSolutionRandom = new Random(initialSolutionSeed)
      // Not seeded: only diversifies the exploration, does not affect the model
      val searchRandom = new Random()
      // ...
    }
  }
  ```

  Note the distinction: randomness that shapes the **model** must be reproducible, randomness that
  only drives the **exploration** may, and usually should, differ from one node to the next.
- **A clock reading, a `UUID`, a hostname** or any other environment-dependent value.
- **Iterating over a `HashMap`/`HashSet`** whose order is not stable, when that order decides in
  which order the variables are created.

This holds within a single JVM too: the supervisor calls `buildLocalSearchModel` once for itself and
once per local worker it spawns, and those models must agree just as much as the remote ones do.
`VRPPopulationProblemStatement` in `VRPDistributedPopulationBasedMultiJVMExample.scala` shows the
seeded pattern in full.

### Supervisor Node

```scala
object MySupervisorNode {
  def main(args: Array[String]): Unit = {
    val supervisorHost = "192.168.1.100"  // Your machine's IP
    val supervisorPort = 2551

    // Create problem statement with problem data
    val problemStatement = MyProblemStatement(problemData)

    // Build supervisor's model
    val (store, obj, searches) = problemStatement.buildLocalSearchModel()
    val search = searches.head

    // Create distributed search with cluster mode enabled
    val distributedSearch = DistributedSearch(
      store,
      verbose = 1,
      distributed = true,  // Enable cluster mode
      problemStatement = Some(problemStatement),
      supervisorHost = supervisorHost,
      supervisorPort = supervisorPort,
      search = searches: _*
    )

    println(s"Supervisor address: ${distributedSearch.supervisorAddress}")
    println("Waiting for workers to connect...")

    // Optionally spawn local workers
    for (_ <- 0 until 2) {
      val (workerStore, _, workerSearches) = problemStatement.buildLocalSearchModel()
      distributedSearch.spawnLocalWorker(workerStore, workerSearches.head)
    }

    // Wait for the expected workers, local and remote alike, to register to the supervisor.
    // It returns as soon as they are all there, and after at most 10 seconds otherwise.
    val nbConnectedWorkers = distributedSearch.waitForWorkers(nbWorkers = 6, timeout = 10.seconds)
    println(s"$nbConnectedWorkers workers registered")

    // Run optimization
    search.doAllMoves(obj)

    distributedSearch.globalShutDown()
  }
}
```

### Worker Node

```scala
object MyWorkerNode {
  def main(args: Array[String]): Unit = {
    val supervisorHost = "192.168.1.100"  // Supervisor's IP
    val supervisorPort = 2551
    val workerHost = "192.168.1.101"  // This machine's IP
    val nbWorkers = Runtime.getRuntime.availableProcessors() / 4

    // Create worker node that connects to supervisor
    // Workers receive ProblemStatement and call buildLocalSearchModel()
    val workerNode = DistributedSearch(
      supervisorHost = supervisorHost,
      supervisorPort = supervisorPort,
      workerHost = workerHost,
      nbWorkers = nbWorkers,
      verbose = 1
    )

    println("Waiting for problem data from supervisor...")
    workerNode.awaitTermination()
  }
}
```

### Key Points

1. **Network Configuration**: Supervisor and workers need proper network connectivity
2. **Serialization**: Uses Kryo serialization for efficient message passing
3. **Problem Statement**: Contains all data needed to reconstruct the model on workers
4. **Determinism**: `buildLocalSearchModel()` must build the same model from the same statement on every node, seeds included (see above)
5. **Fault Tolerance**: Workers can join/leave dynamically; Death Watch detects failures

## The DistributedModulo Combinator

`DistributedModulo` is a combinator for parallel neighborhood exploration. It splits the search space using a modulo pattern.

### How It Works

Given N workers, the neighborhood space is split into N parts:
- Worker 0 explores indices: {0, N, 2N, 3N, ...}
- Worker 1 explores indices: {1, N+1, 2N+1, 3N+1, ...}
- Worker k explores indices: {k, N+k, 2N+k, 3N+k, ...}

This provides good load balancing even when neighbor costs vary.

### Parameters

```scala
DistributedModulo(
  moduloNeighborhood: ModuloRange => Neighborhood,  // Factory function
  nbWorkerToNbClusters: Int => Int = a => a,        // Customize split count
  first: Boolean = true                             // First vs Best move
)
```

### Alternative usage: Distributed Static Split

For a fixed number of neighborhoods, DistributedModulo can be used to configure the parallel execution of all the neighborhoods over the same search space. The parameter named 'first' is a flag to indicate whether the commited move is the first one or the best one:

```scala
DistributedModulo.distributed(
  neighborhoods = List(neighborhood1, neighborhood2, neighborhood3),
  first = true
)
```

## The DistributedPopulationBased Combinator

`DistributedPopulationBased[D]` is a population-based meta-heuristic combinator that distributes the diversification of individuals across workers. Unlike `DistributedModulo`, which parallelizes the exploration of a single neighborhood, `DistributedPopulationBased` maintains a population of solutions — each carrying user-defined data of type `D` — and evolves them over multiple iterations.

It is the distributed counterpart of the sequential `PopulationBasedSearch[D]` combinator. The data `D` stays entirely on the supervisor and is never serialized to workers.

### How It Works

1. **Initialization**: The current solution becomes the initial population (a single individual with data from `initData()`)
2. **Diversification**: The `step` function is called with the iteration number and the data of all individuals. It returns a `genChildren` function that, for each individual, decides which neighborhoods to apply and what data to associate with each child. Each (individual, neighborhood) task is distributed to a worker that runs `doAllMoves`
3. **Selection**: All candidate solutions (and optionally the parents) are sorted by objective value, filtered for redundancy, and the best `nbToKeep` are selected. An optional alternate objective can be used for both generation and selection
4. **Iteration**: Steps 2-3 repeat until the `step` function returns `None` or `maxIt` is reached
5. **Result**: The best solution found is returned, only if it improves over the initial solution

```
Iteration 0:         [S0]
                    / | \       (diversify with neighborhoods chosen by genChildren, on workers)
Iteration 1:   [S1, S2, S3]    (select best nbToKeep)
               / | \ / | \     (diversify again, per-individual neighborhood selection)
Iteration 2:   [S4, S5, ...]   (select best nbToKeep)
               ...
Final:          best solution
```

### Two APIs

`DistributedPopulationBased` provides two factory methods:

#### Full API: `DistributedPopulationBased.apply[D]`

The full API mirrors the sequential `PopulationBasedSearch[D]`, with per-individual data, per-individual neighborhood selection, and optional alternate objective:

```scala
DistributedPopulationBased[D](
  neighborhoods: Array[Neighborhood],    // Available neighborhoods; genChildren must return elements of this array
  initData: () => D,                     // Generates data for the initial individual
  step: (Int, List[D]) => Option[        // (iteration, dataOfAllIndividuals) =>
    (                                    //   Some(...) to continue, None to stop
      (Solution, D) => (Boolean, List[(Neighborhood, D)]),  // genChildren: (solution, data) => (keepOld, children)
      Int,                               //   nbToKeep
      Option[Objective]                  //   optional alternate objective for generation/selection
    )
  ],
  store: Store,                          // The store for solution management
  maxIt: Int = Int.MaxValue,             // Maximum iterations
  saveAnytimeBest: Boolean = false,      // Preserve best solution ever found
  filterRedundantElements: Boolean = true, // Remove duplicate solutions
  dropIfNoMoveFound: Boolean = false,    // Drop individual if no improvement found
  name: String = "DistributedPopulationBased"
)
```

The `genChildren` function returned by `step` receives the [[Solution]] and data of an individual, and returns:
- `keepOld: Boolean` — whether to keep the parent in the next generation
- `List[(Neighborhood, D)]` — a list of `(neighborhood, childData)` pairs specifying which neighborhoods to apply and the data for each resulting child

The returned `Neighborhood` objects must be elements of the `neighborhoods` array passed to the combinator (compared by reference equality). The supervisor internally maps each returned neighborhood back to its index to dispatch the task to a worker. This allows per-individual neighborhood selection: different individuals can be diversified with different subsets of neighborhoods.

The `Solution` passed to `genChildren` is attached to the supervisor's `store`, so the user can call `restoreSolution()` on it to inspect variable values when deciding which neighborhoods to apply.

**Mapping from sequential `PopulationBasedSearch[D]`:**

| Sequential | Distributed | Reason |
|---|---|---|
| `initData: () => D` | Same | `D` stays on supervisor |
| `step` receives `List[D]` | Same | `D` stays on supervisor |
| `genChildren` receives `(Solution, D)` | Same | The supervisor attaches each individual's solution to its own store before the call |
| `genChildren` returns `List[(Neighborhood, D)]` | Same | Returned neighborhoods must come from the pre-registered `neighborhoods` array (reference equality); they are mapped back to an index internally |
| `Option[Objective]` for generation+selection | Same | Workers use it for generation; supervisor uses it for selection |

#### Simplified API: `DistributedPopulationBased.simple`

For cases where per-individual data is not needed, the simplified API uses `Unit` as the data type and applies all neighborhoods uniformly to every individual:

```scala
DistributedPopulationBased.simple(
  neighborhoods: Array[Neighborhood],    // Neighborhoods for diversification
  step: (Int, Int) => Option[Int],       // (iteration, popSize) => Some(nbToKeep) or None to stop
  store: Store,                          // The store for solution management
  maxIt: Int = Int.MaxValue,             // Maximum iterations
  saveAnytimeBest: Boolean = false,      // Preserve best solution ever found
  filterRedundantElements: Boolean = true, // Remove duplicate solutions
  dropIfNoMoveFound: Boolean = false,    // Drop individual if no improvement found
  keepOld: Boolean = false               // Keep parent alongside children
)
```

### Key Parameters

- **`neighborhoods`**: The set of available neighborhoods. In the full API, `genChildren` selects which to apply per individual by returning neighborhoods from this array. In the simple API, all are applied to every individual
- **`step`**: Controls termination and population management. For example, `(it, _) => if (it < 10) Some(5) else None` runs 10 iterations keeping 5 best individuals
- **`initData`** (full API): Generates data for the initial individual. This data flows through the population and can guide neighborhood selection per individual
- **`Option[Objective]`** (full API): When provided in the step return, workers optimize for this alternate objective during `doAllMoves` (generation), and the supervisor sorts/selects individuals by it (selection). The main objective is still used for anytime-best tracking and final result comparison
- **`saveAnytimeBest`**: When `true`, the best solution ever encountered (by the main objective) is preserved even if later iterations produce worse results
- **`keepOld`**: (simple API) When `true`, parent solutions are included alongside children before selection. In the full API, this is controlled per-individual by the `genChildren` return value
- **`filterRedundantElements`**: When `true`, identical solutions are removed before selection to maintain population diversity

### Usage Pattern: Simple API (Single JVM)

```scala
import oscar.cbls._
import oscar.cbls.core.distributed.DistributedSearch
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedPopulationBased

def createModelAndSearch: (Store, Objective, Neighborhood) = {
  implicit val m: Model = model("My Problem")

  // Define variables, constraints, objective...
  val myVars = Array.tabulate(n)(i => binaryVar(0, name = s"var_$i"))
  val obj = m.minimize(objectiveExpr)
  m.close()

  // Define diversification neighborhoods
  val search = DistributedPopulationBased.simple(
    neighborhoods = Array(
      Nrs.combinator.exhaust(List(
        Nrs.assign(myVars, hotRestart = true, name = "assign1"),
        Nrs.swap(myVars, hotRestart = true, name = "swap1")
      )),
      Nrs.combinator.exhaust(List(
        Nrs.swap(myVars, hotRestart = true, name = "swap2"),
        Nrs.assign(myVars, hotRestart = true, name = "assign2")
      ))
    ),
    step = (it, _) => if (it < 10) Some(5) else None,
    store = m.store,
    saveAnytimeBest = true,
    keepOld = true
  )

  (m.store, obj, search)
}

// Create the supervisor
val (store, obj, search) = createModelAndSearch
val distributedSearch = DistributedSearch(store, verbose = 0, search = search)

// Spawn workers
for (_ <- 0 until DistributedSearch.nbCores / 4) {
  val (workerStore, _, workerSearch) = createModelAndSearch
  distributedSearch.spawnLocalWorker(workerStore, workerSearch)
}

search.doAllMoves(obj)
distributedSearch.globalShutDown()
```

### Usage Pattern: Full API with Data

The full API allows per-individual data to guide neighborhood selection, similar to the sequential `PopulationBasedSearch[D]`:

```scala
// Data type: Boolean indicating which variable group to focus on
val neighborhoods = Array(neighborhoodA, neighborhoodB, neighborhoodC)

val search = DistributedPopulationBased[Boolean](
  neighborhoods = neighborhoods,
  initData = () => true,
  step = (it, dataList) => {
    if (it >= 10) None
    else {
      val genChildren: (Solution, Boolean) => (Boolean, List[(Neighborhood, Boolean)]) =
        (solution, data) => {
          // Per-individual neighborhood selection based on data
          // (optionally, call solution.restoreSolution() and inspect variables)
          val children =
            if (data) List((neighborhoods(0), true), (neighborhoods(1), true))
            else List((neighborhoods(1), false), (neighborhoods(2), false))
          (false, children)  // keepOld = false
        }
      Some((genChildren, 4, None))
    }
  },
  store = m.store,
  saveAnytimeBest = false
)
```

### Multi-JVM Usage

The multi-JVM setup follows the same pattern as `DistributedModulo` (see above), using a `ProblemStatement` to transmit problem data to workers. The `buildLocalSearchModel()` method creates a `DistributedPopulationBased` combinator on each node:

```scala
case class MyPopulationProblemStatement(
  problemData: Array[Long]
) extends ProblemStatement {

  override def buildLocalSearchModel(): (Store, Objective, Seq[Neighborhood]) = {
    implicit val m: Model = model("My Problem")

    // Build the model using problemData...
    val vars = Array.tabulate(problemData.length)(i =>
      binaryVar(0, name = s"var_$i")
    )
    val obj = m.minimize(objectiveExpr)
    m.close()

    val search = DistributedPopulationBased.simple(
      neighborhoods = Array(/* ... */),
      step = (it, _) => if (it < 10) Some(5) else None,
      store = m.store,
      saveAnytimeBest = true,
      keepOld = true
    )

    (m.store, obj, Seq(search))
  }
}
```

The supervisor and worker nodes are set up identically to the `DistributedModulo` multi-JVM pattern.

### Design Tips

- **Diverse neighborhoods**: Using neighborhoods that explore the search space differently (e.g., different ordering of assign/swap) produces a more diverse population
- **Randomized neighborhoods**: Workers with different `Random` instances produce diverse solutions even with the same neighborhood definition. This applies to the `Random` that drives the *exploration* only: the one used to build the *model* itself must be seeded from the problem statement, otherwise the nodes end up with inconsistent models (see [`buildLocalSearchModel` must be deterministic](#buildlocalsearchmodel-must-be-deterministic))
- **Population sizing**: `nbToKeep` should balance diversity (larger) with convergence speed (smaller)
- **Per-individual data**: Use the full `[D]` API when different individuals should be diversified differently (e.g., focusing on different variable groups or using different search strategies based on solution characteristics)
- **Alternate objective**: Use `Option[Objective]` when you want generation and selection to optimize for a different criterion than the main objective (e.g., diversification-oriented objectives)
- **`keepOld` / per-individual keepOld**: Useful when diversification may temporarily worsen solutions; parents survive into the next round
- **`saveAnytimeBest = true`**: Recommended when the population can degrade over time, ensures the best solution is never lost

## Choosing Between DistributedModulo and DistributedPopulationBased

| Aspect | DistributedModulo | DistributedPopulationBased |
|--------|-------------------|---------------------------|
| **Strategy** | Splits one neighborhood across workers | Evolves a population of solutions using workers |
| **Parallelism** | Within a single neighborhood exploration | Across independent diversification runs |
| **Task type** | `GetModuloMove` (partial search) | `DoAllMoves` (complete search) |
| **Result** | One move (first or best) per exploration | Best solution after multiple iterations |
| **Per-individual data** | No | Yes (`[D]` type parameter, stays on supervisor) |
| **Per-individual neighborhood selection** | No | Yes (via `genChildren` in full API) |
| **Alternate objective** | No | Yes (`Option[Objective]` for generation and selection) |
| **Use case** | Large neighborhoods expensive to explore sequentially | Meta-heuristic search with population diversity |
| **Worker granularity** | Fine-grained (each worker explores a slice) | Coarse-grained (each worker runs a full `doAllMoves`) |

**Use `DistributedModulo`** when you have a single expensive neighborhood and want to speed up each exploration step by splitting the work.

**Use `DistributedPopulationBased`** when you want to explore multiple solution trajectories in parallel and select the best outcomes, especially when randomization or different neighborhood orderings can produce diverse solutions.

## Comparison: Sequential vs Distributed

### Sequential (WLPAdvancedModelingExample)

```scala
implicit val m: Model = model("WLP")
// ... model definition ...
m.close()

val search = Nrs.assign(facilitiesVariables)
search.doAllMoves(obj)
```

- Single Store, single thread
- Simple setup
- No parallelism

### Distributed Single JVM (WLPDistributedExample)

```scala
def createModelAndSearch = { /* ... */ }

val (store, obj, search) = createModelAndSearch
val distributedSearch = DistributedSearch(store, verbose = 0, search = search)

for (_ <- 0 until nbCores) {
  val (s, _, n) = createModelAndSearch
  distributedSearch.spawnLocalWorker(s, n)
}

search.doAllMoves(obj)
distributedSearch.globalShutDown()
```

- Multiple Stores, multiple threads
- Moderate complexity
- Local parallelism on multi-core machines

### Distributed Multi-JVM (WLPDistributedMultiJVMExample)

```scala
// Supervisor
val problemStatement = WLPProblemStatement(data)
val distributedSearch = DistributedSearch(
  store, verbose = 1, distributed = true,
  problemStatement = Some(problemStatement),
  supervisorHost = host, supervisorPort = port,
  search = searches: _*
)

// Worker (separate JVM/machine)
val workerNode = DistributedSearch(
  supervisorHost = supervisorHost,
  supervisorPort = supervisorPort,
  workerHost = workerHost,
  nbWorkers = n, verbose = 1
)
```

- Multiple JVMs, potentially across machines
- More complex setup
- Full distributed computing capability

## Best Practices

1. **Choose the right number of workers**: `DistributedSearch.nbCores / 4` is a reasonable starting point
2. **Use modulo splitting**: Provides natural load balancing
3. **Keep tasks granular**: Fine-grained tasks allow better distribution
4. **Consider network overhead**: In multi-JVM mode, minimize data transfer
5. **Handle failures gracefully**: The framework tolerates worker disconnections
6. **Keep model building deterministic**: `buildLocalSearchModel()` must be a pure function of the problem statement, seeds included

## Performance Considerations

- **Overhead**: Distributed search has communication overhead; use for expensive neighborhoods
- **Scalability**: Linear speedup is ideal but rarely achieved due to synchronization
- **Memory**: Each worker needs its own Store; consider memory limits
- **Network**: In multi-JVM mode, network latency affects performance

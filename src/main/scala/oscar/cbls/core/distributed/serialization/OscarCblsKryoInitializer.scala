package oscar.cbls.core.distributed.serialization

import com.esotericsoftware.kryo.kryo5.{Kryo, Serializer}
import com.esotericsoftware.kryo.kryo5.io.{Input, Output}
import io.altoo.serialization.kryo.scala.DefaultKryoInitializer
import io.altoo.serialization.kryo.scala.serializer.ScalaKryo
import org.apache.pekko.actor.typed.{ActorRef, ActorRefResolver, ActorSystem}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.serialization.Serialization
import oscar.cbls.core.distributed.computation._
import oscar.cbls.core.distributed.protocol._

/**
 * Custom Kryo initializer that registers all serializable classes
 * used in the Oscar-CBLS distributed search protocol.
 *
 * This initializer is referenced in the Pekko configuration under:
 * pekko-kryo-serialization.kryo-initializer
 *
 * IMPORTANT: This initializer includes a custom ActorRef serializer
 * that properly serializes actor references as path strings using
 * Pekko's ActorRefResolver, rather than trying to serialize the
 * entire actor system graph via reflection.
 */
class OscarCblsKryoInitializer extends DefaultKryoInitializer {

  override def postInit(kryo: ScalaKryo): Unit = {
    super.postInit(kryo)

    // =====================================================
    // CRITICAL: Register custom ActorRef serializers
    // This serializes ActorRefs as path strings instead of
    // trying to serialize the entire actor system graph.
    //
    // We register serializers for the public ActorRef types.
    // Kryo will use these for any subclass implementations.
    // =====================================================
    kryo.addDefaultSerializer(classOf[org.apache.pekko.actor.typed.ActorRef[_]],
      new TypedActorRefSerializer())
    kryo.addDefaultSerializer(classOf[org.apache.pekko.actor.ActorRef],
      new ClassicActorRefSerializer())

    // =====================================================
    // ProblemStatement abstract class
    // Note: Concrete subclasses must be registered separately.
    // To register your own ProblemStatement subclass, create a custom
    // Kryo initializer that extends this one and call:
    //   kryo.register(classOf[YourProblemStatement])
    // in the postInit method, then reference your initializer in your
    // Pekko configuration under pekko-kryo-serialization.kryo-initializer
    // =====================================================
    kryo.register(classOf[ProblemStatement])

    // =====================================================
    // Protocol Messages - MessageToWorker hierarchy
    // =====================================================
    kryo.register(classOf[EnqueueTask])
    kryo.register(classOf[KillTasks])
    kryo.register(classOf[WorkerStatusRequest])
    kryo.register(classOf[WorkerShutdown])

    // =====================================================
    // Protocol Messages - MessageToSupervisor hierarchy
    // =====================================================
    kryo.register(GlobalShutDown.getClass)
    kryo.register(classOf[WorkerRegister])
    kryo.register(classOf[WorkerTaskFinished])
    kryo.register(classOf[WorkerTaskCancelled])
    kryo.register(classOf[WorkerStatusReport])
    kryo.register(classOf[WorkerCrash])
    kryo.register(classOf[CreateTask])
    kryo.register(classOf[CreateTasks])
    kryo.register(classOf[CancelTask])
    kryo.register(classOf[CancelAllMyRemainingTasks])
    kryo.register(classOf[GetNewUniqueTaskIds])
    kryo.register(classOf[SpawnLocalWorker])

    // =====================================================
    // Protocol Messages - MessageToSearch hierarchy
    // =====================================================
    kryo.register(classOf[NewUniqueTaskIds])
    kryo.register(classOf[StatusReport])
    kryo.register(classOf[ResultObtained])
    kryo.register(classOf[Crash])

    // =====================================================
    // Task and TaskResult
    // =====================================================
    kryo.register(classOf[Task])
    kryo.register(classOf[TaskResult])

    // =====================================================
    // TaskParameters hierarchy
    // =====================================================
    kryo.register(classOf[GetMove])
    kryo.register(classOf[DoAllMoves])
    kryo.register(classOf[ProgressReport])

    // =====================================================
    // ActualResult hierarchy
    // =====================================================
    kryo.register(Aborted.getClass)
    kryo.register(classOf[TaskResultMove])
    kryo.register(TaskResultNoMoveFound.getClass)

    // =====================================================
    // StoreIndependent classes (serializable model state)
    // =====================================================
    kryo.register(classOf[StoreIndependentSolution])
    kryo.register(classOf[StoreIndependentIntSavedValue])
    kryo.register(classOf[StoreIndependentSetSavedValue])
    kryo.register(classOf[StoreIndependentSeqSavedValue])
    kryo.register(classOf[StoreIndependentLoadSolutionMove])

    // StoreIndependentObjective implementations
    kryo.register(classOf[StoreIndependentMinimize])
    kryo.register(classOf[StoreIndependentMaximize])

    // =====================================================
    // Scala Standard Library types used in messages
    // =====================================================
    kryo.register(classOf[List[_]])
    kryo.register(classOf[Set[_]])
    kryo.register(classOf[Map[_, _]])
    kryo.register(classOf[Vector[_]])
    kryo.register(classOf[scala.collection.immutable.HashSet[_]])
    kryo.register(classOf[scala.collection.immutable.HashMap[_, _]])

    kryo.register(classOf[Option[_]])
    kryo.register(classOf[Some[_]])
    kryo.register(None.getClass)

    kryo.register(classOf[Array[Int]])
    kryo.register(classOf[Array[Long]])
    kryo.register(classOf[Array[Byte]])
    kryo.register(classOf[Array[Object]])

    kryo.register(classOf[(_, _)])
    kryo.register(classOf[(_, _, _)])
    kryo.register(classOf[(_, _, _, _)])

    kryo.register(classOf[java.lang.Long])
    kryo.register(classOf[java.lang.Integer])
    kryo.register(classOf[java.lang.String])
    kryo.register(classOf[java.util.ArrayList[_]])
  }
}

/**
 * Custom Kryo serializer for Pekko Typed ActorRef.
 *
 * This serializer converts ActorRefs to/from their string path representation
 * using Pekko's serialization system, which properly handles remote actor references.
 *
 * IMPORTANT: This serializer writes the ActorRef as a plain string, avoiding
 * the need for Kryo to register any ActorRef implementation classes.
 */
class TypedActorRefSerializer extends Serializer[ActorRef[_]] {

  // Disable references for this serializer - ActorRef paths are self-contained strings
  setAcceptsNull(false)
  setImmutable(true)

  override def write(kryo: Kryo, output: Output, actorRef: ActorRef[_]): Unit = {
    // Get the serialization info from Pekko's thread-local context
    val info = Serialization.getCurrentTransportInformation()
    val path = if (info != null) {
      // Use the actor system from serialization context
      ActorRefResolver(info.system.toTyped).toSerializationFormat(actorRef)
    } else {
      // Fallback: just use the path (might not work for remote refs)
      actorRef.path.toSerializationFormat
    }
    // Write as a simple string - no class registration needed
    output.writeString(path)
  }

  override def read(kryo: Kryo, input: Input, clazz: Class[_ <: ActorRef[_]]): ActorRef[_] = {
    val path = input.readString()
    val info = Serialization.getCurrentTransportInformation()
    if (info != null) {
      ActorRefResolver(info.system.toTyped).resolveActorRef(path)
    } else {
      throw new IllegalStateException(
        "Cannot deserialize ActorRef without Pekko serialization context. " +
          "Make sure Pekko remoting is properly configured.")
    }
  }
}

/**
 * Custom Kryo serializer for Pekko Classic ActorRef.
 *
 * This serializer converts classic ActorRefs to/from their string path representation.
 * For deserialization, we use the typed ActorRefResolver and convert back to classic.
 *
 * IMPORTANT: This serializer writes the ActorRef as a plain string, avoiding
 * the need for Kryo to register any ActorRef implementation classes.
 */
class ClassicActorRefSerializer extends Serializer[org.apache.pekko.actor.ActorRef] {

  // Disable references for this serializer - ActorRef paths are self-contained strings
  setAcceptsNull(false)
  setImmutable(true)

  override def write(kryo: Kryo, output: Output, actorRef: org.apache.pekko.actor.ActorRef): Unit = {
    val path = Serialization.serializedActorPath(actorRef)
    // Write as a simple string - no class registration needed
    output.writeString(path)
  }

  override def read(kryo: Kryo, input: Input, clazz: Class[_ <: org.apache.pekko.actor.ActorRef]): org.apache.pekko.actor.ActorRef = {
    val path = input.readString()
    val info = Serialization.getCurrentTransportInformation()
    if (info != null) {
      // Use the typed ActorRefResolver to resolve the path, then convert to classic
      val typedSystem: ActorSystem[Nothing] = info.system.toTyped
      val resolver = ActorRefResolver(typedSystem)
      // Resolve as typed ActorRef[Any] and convert to classic
      val typedRef = resolver.resolveActorRef[Any](path)
      import org.apache.pekko.actor.typed.scaladsl.adapter._
      typedRef.toClassic
    } else {
      throw new IllegalStateException(
        "Cannot deserialize ActorRef without Pekko serialization context. " +
          "Make sure Pekko remoting is properly configured.")
    }
  }
}
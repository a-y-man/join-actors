package new_benchmarks.bounded_buffer

import join_actors.actor.ActorRef
import join_patterns.matching.MatchingAlgorithm

import scala.concurrent.Promise

// Termination message to stop the actors
case class TerminateActors()

type BufferType = String

// Bounded buffer events
enum ExternalEvent:
  case Put(ref: ProducerSyncReply, x: BufferType)
  case Get(ref: ConsumerReply)

// Internal events
enum InternalEvent:
  case P(x: BufferType)
  case Free(x: Int)
  case Full()

// Response to Get event
type ConsumerReply = Promise[BufferType]

// Response to Put event
type ProducerSyncReply = Promise[Unit]

type BBEvent = ExternalEvent | InternalEvent | ConsumerReply | TerminateActors

type BBRef = ActorRef[BBEvent]

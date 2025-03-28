package new_benchmarks.bounded_buffer

import join_actors.api.*
import new_benchmarks.{Benchmark, BenchmarkFactory}
import new_benchmarks.bounded_buffer.BoundedBuffer.*
import InternalEvent.*
import ExternalEvent.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

class BoundedBuffer(private val algorithm: MatchingAlgorithm, private val config: Config) extends Benchmark[PassPrereqs]:
  override def prepare(param: Int): PassPrereqs =
    val bb = getBoundedBufferActor(algorithm)

    val (result, ref) = bb.start()

    (param, result, ref)

  override def run(passConfig: PassPrereqs): Unit =
    val (nProdsCons, result, ref) = passConfig

    ref ! Free(config.bufferBound)
    coordinator(
      ref,
      config,
      nProdsCons
    ) // This blocks until all producers and consumers are done

    Await.result(result, Duration(10, TimeUnit.MINUTES))

  private def getBoundedBufferActor(algorithm: MatchingAlgorithm): Actor[BBEvent, (Long, Int)] =
    import InternalEvent.*
    import ExternalEvent.*
    var matches = 0
    Actor[BBEvent, (Long, Int)] {
      receive { (bbRef: BBRef) => {
        case (Put(producerRef, x), Free(c)) =>
          if c == 1 then bbRef ! Full()
          else bbRef ! Free(c - 1)
          bbRef ! P(x)
          producerRef.success(())
          Continue
        case (Get(consumerRef), P(x), Full()) =>
          bbRef ! Free(1)
          consumerRef.success(x)
          matches += 1
          Continue
        case (Get(consumerRef), P(x), Free(c)) =>
          bbRef ! Free(c + 1)
          consumerRef.success(x)
          matches += 1
          Continue
        case TerminateActors() =>
          Stop((System.currentTimeMillis(), matches))
      }
      }(algorithm)
    }

  private def coordinator(
    bbRef: BBRef,
    bbConfig: BoundedBufferConfig,
    nProdsCons: Int
  ): Unit =
    val msg = "hello"

    def producer() =
      Future {
        for i <- 0 until bbConfig.count do
          val prodPromise: ProducerSyncReply = Promise[Unit]()
          val prodFut = prodPromise.future
          bbRef ! Put(prodPromise, msg)
          Await.ready(prodFut, Duration(10, TimeUnit.MINUTES))
      }

    def consumer() =
      Future {
        for _ <- 0 until bbConfig.count do
          val consPromise: ConsumerReply = Promise[BufferType]()
          val consFut = consPromise.future
          bbRef ! Get(consPromise)
          Await.ready(consFut, Duration(10, TimeUnit.MINUTES))
      }

    // Create and start consumers
    val consumers = Future.sequence((0 until nProdsCons).map(_ => consumer()))

    // Create and start producers
    val producers = Future.sequence((0 until nProdsCons).map(_ => producer()))

    Await.ready(Future.sequence(Seq(consumers, producers)), Duration(10, TimeUnit.MINUTES))

    bbRef ! TerminateActors()

object BoundedBuffer extends BenchmarkFactory:
  override type Config = BoundedBufferConfig
  override type PassPrereqs = (Int, Future[(Long, Int)], ActorRef[BBEvent])
  override type InstanceType = BoundedBuffer

  override def apply(algorithm: MatchingAlgorithm, config: BoundedBufferConfig): BoundedBuffer = new BoundedBuffer(algorithm, config)

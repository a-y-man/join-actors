package benchmarks

import examples.JBB
import examples.boundedBufferBench
import join_patterns.MatchingAlgorithm
import join_patterns.MatchingAlgorithm.BruteForceAlgorithm
import join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm
import joins.*

import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.*

import events.*

class JProducer(b: JBB, cnt: Int, prom: Promise[Boolean]) extends Runnable:
  val s = "hello"
  def run(): Unit =
    for _ <- List.range(0, cnt) do b.Put(s)
    prom.success(true)

class JConsumer(b: JBB, cnt: Int, prom: Promise[Boolean]) extends Runnable:
  def run(): Unit =
    for _ <- List.range(0, cnt) do b.Get()
    prom.success(true)

// def measureJBB(num: Int, times: Int, jbuf: JBB) =
//   println("testing join-based implementation...")
//   val jstart   = System.currentTimeMillis
//   var jthreads = List[Thread]()
//   var jfuts    = List[Future[Boolean]]()

//   for i <- List.range(0, num) do
//     val prodProm = Promise[Boolean]()
//     val jprod    = Thread.ofVirtual().unstarted(new JProducer(jbuf, times, prodProm))
//     val consProm = Promise[Boolean]()
//     val jcons    = Thread.ofVirtual().unstarted(new JConsumer(jbuf, times, consProm))
//     jthreads = jprod :: jcons :: jthreads
//     jfuts = prodProm.future :: consProm.future :: jfuts
//     jprod.start()
//     jcons.start()

//   jfuts foreach { fut => Await.ready(fut, 10.seconds) }

// val jend = System.currentTimeMillis
// jthreads foreach { t => t.join() }
// println(s"time: ${Duration(jend - jstart, TimeUnit.MILLISECONDS)}")

def measureJBB(bbConfig: BBConfig) =
  val jbuf = new JBB:
    this.Free(bbConfig.bufferBound)

  def startConsumers(jbuf: JBB) =
    (1 to bbConfig.consumers).map { _ =>
      val consProm = Promise[Boolean]()
      val jcons    = Thread.ofVirtual().unstarted(new JConsumer(jbuf, bbConfig.cnt, consProm))
      (jcons, consProm.future)
    }.toArray

  def startProds(jbuf: JBB) =
    (1 to bbConfig.producers).map { _ =>
      val prodProm = Promise[Boolean]()
      val jprod    = Thread.ofVirtual().unstarted(new JProducer(jbuf, bbConfig.cnt, prodProm))
      (jprod, prodProm.future)
    }.toArray

  val threads = startConsumers(jbuf) ++ startProds(jbuf)
  Future {
    val jstart = System.currentTimeMillis
    threads.foreach { case (t, _) => t.start() }
    threads.foreach { case (_, fut) => Await.ready(fut, 10.seconds) }
    val jend = System.currentTimeMillis
    threads.foreach { case (t, _) => t.join() }
    Measurement(
      jend - jstart,
      bbConfig.cnt * bbConfig.producers
    )
  }

def scalaJoinsBoundedBufferBenchmark(
    bbConfigs: Array[MatchingAlgorithm => BBConfig],
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =
  val warmupConfig =
    BBConfig(
      bufferBound = 16,
      producers = 8,
      consumers = 8,
      cnt = 16,
      algorithm = BruteForceAlgorithm
    )

  Benchmark(
    name = "ScalaJoins Bounded Buffer",
    algorithm = BruteForceAlgorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      s"Null Pass ${BruteForceAlgorithm}",
      () => measureJBB(warmupConfig)
    ),
    passes = bbConfigs.map { config =>
      val bbConfig = config(BruteForceAlgorithm)
      BenchmarkPass(
        s"${bbConfig.bufferBound} ${bbConfig.producers} ${bbConfig.consumers} ${bbConfig.cnt}",
        () => measureJBB(bbConfig)
      )
    }
  )

def runScalaJoinsBBBenchmark(
    bufferBound: Int,
    nProdsCons: Int,
    writeToFile: Boolean,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =
  val bbConfigs =
    Array((1 to nProdsCons).map(n => (bufferBound, n))*).map { case (bufferBound, nProdsCons) =>
      (algo: MatchingAlgorithm) =>
        BBConfig(
          bufferBound = bufferBound,
          producers = nProdsCons,
          consumers = nProdsCons,
          cnt = bufferBound,
          algorithm = algo
        )
    }

  println(
    s"${Console.GREEN}${Console.UNDERLINED}Running ScalaJoins JBB benchmark${Console.RESET}"
  )
  val m =
    scalaJoinsBoundedBufferBenchmark(
      bbConfigs,
      warmupRepetitions,
      repetitions
    ).run()
  println(
    s"${Console.RED}${Console.UNDERLINED}Benchmark ScalaJoins JBB finished${Console.RESET}"
  )

  if writeToFile then saveToFile("ScalaJoinsBoundedBuffer", List((BruteForceAlgorithm, m)))

// object testJBB extends App:
//   import scala.concurrent.Future
//   // import scala.concurrent.ExecutionContext.Implicits.global

//   val buf = new JBB:
//     this.Free(100)

// val s = "hello"
// Future {
//   buf.Put(s)
//   buf.Put(s)
//   println(s"${Thread.currentThread}")
// }
// Future {
//   buf.Get()
//   buf.Get()
// }
// Thread.sleep(500)
// measureJBB(num = 10, times = 100, jbuf = buf)

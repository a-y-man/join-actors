package new_benchmarks.payment

import join_actors.api.*
import join_patterns.util.*
import new_benchmarks.{Benchmark, BenchmarkFactory, intercalate, log}
import new_benchmarks.mixin.MessageFeedBenchmark
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet
import new_benchmarks.payment.Payment.*
import new_benchmarks.payment.PaymentEvent.*

import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Payment(private val algorithm: MatchingAlgorithm) extends Benchmark[PaymentPrereqs]:
  override def prepare(param: Int): PaymentPrereqs =
    val paymentEvents = ArraySeq.fill(param)(ExternalPaymentRequest())
    val tokenEvents = ArraySeq.fill(param)(ExternalTokenGenerationRequest())
    val msgs = paymentEvents.intercalate(tokenEvents) :+ Shutdown()

    val coreServiceCell = RefCell[PaymentActor]()
    val accountServiceCell = RefCell[PaymentActor]()
    val tokenServiceCell = RefCell[PaymentActor]()
    val paymentServiceCell = RefCell[PaymentActor]()

    val (res, coreService: PaymentActor) = getCoreServiceActor(accountServiceCell, tokenServiceCell, paymentServiceCell, param).start()
    val (_, accountService: PaymentActor) = getAccountServiceActor(paymentServiceCell, tokenServiceCell).start()
    val (_, tokenService: PaymentActor) = getTokenServiceActor(accountServiceCell, coreServiceCell).start()
    val (_, paymentService: PaymentActor) = getPaymentServiceActor(coreServiceCell).start()

    coreServiceCell.content = coreService
    accountServiceCell.content = accountService
    tokenServiceCell.content = tokenService
    paymentServiceCell.content = paymentService

    PaymentPrereqs(res, coreService, msgs, ArraySeq(coreService, accountService, tokenService, paymentService))

  override def run(passConfig: PaymentPrereqs): Unit =
    val PaymentPrereqs(result, ref, msgs, cleanup) = passConfig

    for msg <- msgs.fast do
      ref ! msg

    Await.result(result, Duration.Inf)

  private def getPaymentServiceActor(coreService: RefCell[PaymentActor]) =
    val matcher = receive { selfRef => {
      case PaymentRequested(id1)
        &&& MerchantValidated(id2)
        &&& CustomerValidated(id3)
        if (id1 == id2) && (id2 == id3) =>
        coreService.get ! PaymentSuceeded(id1)

        log(s"Payment service handled payment request $id1")
        Continue
      case Shutdown() => Stop(())
    }}(algorithm)

    Actor(matcher)

  private def getAccountServiceActor(paymentService: RefCell[PaymentActor], tokenService: RefCell[PaymentActor]) =
    val matcher = receive { selfRef => {
      case PaymentRequested(id) =>
        paymentService.get ! MerchantValidated(id)

        log(s"Account service validated merchant for payment request $id")
        Continue
      case TokenConsumed(id) =>
        paymentService.get ! CustomerValidated(id)

        log(s"Account service validated customer for payment request $id")
        Continue
      case TokenGenerationRequested(id) =>
        tokenService.get ! CustomerValidated(id)

        log(s"Account service validated customer for token request $id")
        Continue
      case Shutdown() => Stop(())
    }}(algorithm)

    Actor(matcher)

  private def getTokenServiceActor(accountService: RefCell[PaymentActor], coreService: RefCell[PaymentActor]) =
    val matcher = receive { selfRef => {
      case PaymentRequested(id) =>
        accountService.get ! TokenConsumed(id)

        log(s"Token service consumed token for payment request $id")
        Continue
      case TokenGenerationRequested(id1)
        &&& CustomerValidated(id2)
        if id1 == id2 =>
        coreService.get ! TokenGenerated(id1)

        log(s"Token service generated token for token request $id1")
        Continue
      case Shutdown() => Stop(())
    }}(algorithm)

    Actor(matcher)

  private def getCoreServiceActor(
    accountService: RefCell[PaymentActor],
    tokenService: RefCell[PaymentActor],
    paymentService: RefCell[PaymentActor],
    numRequests: Int
  ) =
    var nextId = 1

    var paymentsProcessed = 0
    var tokensProcessed = 0

    val matcher = receive { selfRef => {
      case ExternalPaymentRequest() =>
        val event = PaymentRequested(nextId)
        accountService.get ! event
        paymentService.get ! event
        tokenService.get ! event

        log(s"Core service handled external payment request $nextId")
        nextId += 1
        Continue
      case ExternalTokenGenerationRequest() =>
        val event = TokenGenerationRequested(nextId)
        tokenService.get ! event
        accountService.get ! event

        log(s"Core service handled external token request $nextId")
        nextId += 1
        Continue
      case PaymentSuceeded(id) =>
        paymentsProcessed += 1
        if paymentsProcessed == numRequests && tokensProcessed == numRequests then
          log(s"Core service received last PaymentSucceeded for id $id, shutting down")
          accountService.get ! Shutdown()
          tokenService.get ! Shutdown()
          paymentService.get ! Shutdown()
          Stop(())
        else
          log(s"Core service received PaymentSucceeded for id $id, continuing")
          Continue
      case TokenGenerated(id) =>
        tokensProcessed += 1
        if paymentsProcessed == numRequests && tokensProcessed == numRequests then
          log(s"Core service received last TokenGenerated for id $id, shutting down")
          accountService.get ! Shutdown()
          tokenService.get ! Shutdown()
          paymentService.get ! Shutdown()
          Stop(())
        else
          log(s"Core service received TokenGenerated for id $id, continuing")
          Continue
    }}(algorithm)

    Actor(matcher)

object Payment extends BenchmarkFactory:
  final case class PaymentPrereqs(
    result: Future[Unit],
    coreRef: PaymentActor,
    msgs: ArraySeq[PaymentEvent],
    toCleanup: ArraySeq[PaymentActor]
  )

  override def apply(algorithm: MatchingAlgorithm, config: Unit): Payment = new Payment(algorithm)

  override type Config = Unit
  override type PassPrereqs = PaymentPrereqs
  override type InstanceType = Payment

  private type PaymentActor = ActorRef[PaymentEvent]
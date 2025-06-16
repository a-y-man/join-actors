package core

import examples.payment.Payment
import join_actors.api.MatchingAlgorithm
import join_actors.api.MatchingAlgorithm.*
import join_actors.examples.*
import join_actors.examples.factory_simpl.runFactorySimple
import mainargs.{ParserForClass, ParserForMethods, TokensReader, arg, main}

object Main:
  @main
  case class CommonRunConfig(
    @arg(doc = "The join pattern matching algorithm to use" +
      "Algorithm options: " + MatchingAlgorithm.CMD_STRINGS.mkString(", "))
    algorithm: MatchingAlgorithm = WhileLazyAlgorithm,
  )

  implicit def configParser: ParserForClass[CommonRunConfig] = ParserForClass[CommonRunConfig]

  implicit object MatchingAlgorithmParser extends TokensReader.Simple[MatchingAlgorithm]:
    def shortName: String = "algorithm"
    def read(tokens: Seq[String]): Either[String, MatchingAlgorithm] =
      tokens.headOption.flatMap(MatchingAlgorithm.parseFromCmdString).toRight("Invalid algorithm, should be one of the following: "
        + System.lineSeparator() + MatchingAlgorithm.CMD_STRINGS.mkString(", "))

  @main
  def boundedBuffer(
    commonConfig: CommonRunConfig,
    @arg(short = 'b', doc = "The buffer bound")
    bufferBound: Int = 100,
    @arg(
      short = 'p',
      doc = "The maximum number of producers and consumers"
    )
    count: Int = 50
  ): Unit =
    val bbConfig = BBConfig(
      bufferBound = bufferBound,
      producers = count,
      consumers = count,
      cnt = bufferBound,
      algorithm = commonConfig.algorithm
    )
    runBB(bbConfig)

  @main
  def chameneos(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of meetings")
    maxMeetings: Int = 100,
    @arg(
      short = 'c',
      doc = "The maximum number of chameneos"
    )
    maxChameneos: Int = 50
  ): Unit =
    val chameneosConfig = ChameneosConfig(
      maxNumberOfMeetings = maxMeetings,
      numberOfChameneos = maxChameneos,
      algorithm = commonConfig.algorithm
    )

    chameneosExample(
      chameneosConfig
    )

  @main
  def smartHouse(
      commonConfig: CommonRunConfig,
      @arg(short = 'n', doc = "The number of messages to send")
      messages: Int = 100
  ): Unit =
    val msgs = smartHouseMsgs(messages)(GenerateActions.genActionsOfSizeN)
    runSmartHouseExample(
      commonConfig.algorithm,
      msgs
    )

  @main
  def santaClaus(
      commonConfig: CommonRunConfig,
      @arg(short = 'n', doc = "The number of deliveries to make")
      deliveries: Int = 5
  ): Unit =
    santaClausExample(
      commonConfig.algorithm,
      deliveries
    )

  @main
  def printerSpooler(
      commonConfig: CommonRunConfig,
      @arg(short = 'p', doc = "The number of printers")
      printers: Int = 10,
      @arg(short = 'j', doc = "The number of jobs")
      jobs: Int = 100
  ): Unit =
    printerSpoolerExample(
      commonConfig.algorithm,
      printers,
      jobs
    )

  @main
  def factorySimple(
    commonConfig: CommonRunConfig,
  ): Unit =
    runFactorySimple(commonConfig.algorithm)

  @main
  def payment(
    commonConfig: CommonRunConfig,
    @arg(doc = "The number of payment and token requests sent")
    requests: Int = 30
  ): Unit =
    if requests <= 0 then throw IllegalArgumentException("Number of requests cannot be 0 or less")

    val toRun = Payment(commonConfig.algorithm)

    val prereqs = toRun.prepare(requests)
    toRun.run(prereqs)

  @main
  def simpleExample(
    commonConfig: CommonRunConfig,
  ): Unit =
    exampleFilter(commonConfig.algorithm)

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)

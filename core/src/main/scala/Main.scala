package core

import actor.*
import actor.Result.*
import join_patterns.MatchingAlgorithm
import join_patterns.MatchingAlgorithm.BruteForceAlgorithm
import join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm
import join_patterns.examples.*
import mainargs.Flag
import mainargs.ParserForClass
import mainargs.ParserForMethods
import mainargs.TokensReader
import mainargs.arg
import mainargs.main

// object TestSomething extends App:
//   val msgs = Vector.fill(10)(smartHouseMsgs(0)(GenerateActions.genActionsOfSizeN)).flatten
// println(msgs.mkString(", "))
// runSmartHouseExample(
//   BruteForceAlgorithm,
//   msgs
// )
// println("=====================================================================")
// runSmartHouseExample(
//   StatefulTreeBasedAlgorithm,
//   msgs
// )

object Main:
  implicit object MatchingAlgorithmParser extends TokensReader.Simple[MatchingAlgorithm]:
    def shortName: String = "algorithm"
    def read(tokens: Seq[String]) =
      tokens.headOption match
        case Some("brute")    => Right(BruteForceAlgorithm)
        case Some("stateful") => Right(StatefulTreeBasedAlgorithm)
        case _                => Left("Invalid algorithm")

  @main
  def boundedBuffer(
      @arg(short = 'b', doc = "The buffer bound")
      bufferBound: Int = 100,
      @arg(
        short = 'p',
        doc = "The maximum number of producers and consumers"
      )
      nProdsCons: Int = 50,
      @arg(doc = "The join pattern matching algorithm to use")
      algorithm: MatchingAlgorithm
  ) =
    val bbConfig = BBConfig(
      bufferBound = bufferBound,
      producers = nProdsCons,
      consumers = nProdsCons,
      cnt = bufferBound,
      algorithm = algorithm
    )
    runBB(bbConfig)

  @main
  def chameneos(
      @arg(short = 'm', doc = "The maximum number of meetings")
      maxNumberOfMeetings: Int = 100,
      @arg(
        short = 'c',
        doc = "The maximum number of chameneos"
      )
      nChameneos: Int = 50,
      @arg(doc = "The join pattern matching algorithm to use")
      algorithm: MatchingAlgorithm
  ) =
    val chameneosConfig = ChameneosConfig(
      maxNumberOfMeetings = maxNumberOfMeetings,
      numberOfChameneos = nChameneos,
      algorithm = algorithm
    )

    chameneosExample(
      chameneosConfig
    )

  @main
  def smartHouse(
      @arg(short = 'n', doc = "The number of messages to send")
      nMessages: Int = 100,
      @arg(short = 'a', doc = "The join pattern matching algorithm to use")
      algorithm: MatchingAlgorithm
  ) =
    val msgs = smartHouseMsgs(nMessages)(GenerateActions.genActionsOfSizeN)
    runSmartHouseExample(
      algorithm,
      msgs
    )

  @main
  def santaClaus(
      @arg(short = 'n', doc = "The number of deliveries to make")
      nDeliveries: Int = 100,
      @arg(short = 'a', doc = "The join pattern matching algorithm to use")
      algorithm: MatchingAlgorithm
  ) =
    santaClausExample(
      algorithm,
      nDeliveries
    )

  @main
  def printerSpooler(
      @arg(short = 'p', doc = "The number of printers")
      nPrinters: Int = 10,
      @arg(short = 'j', doc = "The number of jobs")
      nJobs: Int = 100,
      @arg(doc = "The join pattern matching algorithm to use")
      algorithm: MatchingAlgorithm
  ) =
    printerSpoolerExample(
      algorithm,
      nPrinters,
      nJobs
    )

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)

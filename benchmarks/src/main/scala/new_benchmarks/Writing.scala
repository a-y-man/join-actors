package new_benchmarks

import join_patterns.matching.MatchingAlgorithm
import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.chart.ui.{HorizontalAlignment, RectangleInsets}
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.category.{CategoryDataset, DefaultCategoryDataset}
import os.{Path, write}

import java.awt.{BasicStroke, Color, Font, Paint}
import java.text.SimpleDateFormat
import java.util.Date
import scala.concurrent.duration.FiniteDuration

def saveResults(
            benchmarkName: String,
            paramName: String,
            paramRange: Range,
            results: ProcessedBenchmarkSeriesResults,
            dataDir: Path = os.pwd / "benchmarks" / "data"
          ): Unit =
  val timestamp = SimpleDateFormat("yyyy_MM_dd_HH_mm_ss").format(Date())

  saveToFile(benchmarkName, paramName, paramRange, results, dataDir, timestamp)
  saveToPlot(benchmarkName, paramName, paramRange, results, dataDir, timestamp)

private def saveToFile(
            benchmarkName: String,
            paramName: String,
            paramRange: Range,
            results: ProcessedBenchmarkSeriesResults,
            dataDir: Path = os.pwd / "benchmarks" / "data",
            timestamp: String
          ): Unit =
  val algorithmNames = results.map(_._1.toString)
  val headers = paramName +: algorithmNames

  val stringedResults = results.flatMap: (algo, algoResults) =>
    val repetitionColumns = algoResults.head.data.indices.map: repIdx =>
      s"$algo (rep $repIdx)" +: algoResults.map: repRes =>
        repRes.data(repIdx).toString

    val averageColumn = s"$algo avg" +: algoResults.map: repRes =>
      repRes.average.toString

    repetitionColumns :+ averageColumn
  val table = stringedResults.transpose

  val sep = ", "

  val linesToWrite = table.map(_.mkString(sep))
  val strToWrite = linesToWrite.mkString(System.lineSeparator())

  val file = dataDir / f"${timestamp}_${benchmarkName}.csv"

  write(file, strToWrite, createFolders = true)
  println(s"Saved results to $file")

private def saveToPlot(
            benchmarkName: String,
            paramName: String,
            paramRange: Range,
            results: ProcessedBenchmarkSeriesResults,
            dataDir: Path = os.pwd / "benchmarks" / "data",
            timestamp: String
          ): Unit =
  val dataset = makeDatasetFrom(paramRange, results)

  val chart = ChartFactory.createLineChart(benchmarkName, paramName, "Time (ms)", dataset)

  val titleFont = chart.getTitle.getFont.deriveFont(Font.PLAIN, 45f)
  chart.getTitle.setFont(titleFont)
  chart.getTitle.setPadding(15, 15, 15, 15)

  val legend = chart.getLegend()
  val legendFont = legend.getItemFont.deriveFont(20f)
  legend.setItemFont(legendFont)
  legend.setHorizontalAlignment(HorizontalAlignment.CENTER)
  legend.setItemLabelPadding(RectangleInsets(0, 20, 0, 20))

  val plot = chart.getCategoryPlot
  val tickFont = plot.getDomainAxis.getTickLabelFont.deriveFont(Font.PLAIN, 20f)
  val labelFont = plot.getDomainAxis.getLabelFont.deriveFont(Font.PLAIN, 25f)
  println(plot.getDomainAxis.getLabelPaint)
  plot.getDomainAxis.setTickLabelFont(tickFont)
  plot.getRangeAxis.setTickLabelFont(tickFont)
  plot.getDomainAxis.setLabelFont(labelFont)
  plot.getRangeAxis.setLabelFont(labelFont)

  plot.getDomainAxis.setUpperMargin(0)
  plot.getDomainAxis.setLowerMargin(0)

  plot.setBackgroundPaint(Color.WHITE)
  plot.setRangeGridlinePaint(Color.LIGHT_GRAY)
  plot.setRangeGridlineStroke(BasicStroke(3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

  val renderer = plot.getRenderer.asInstanceOf[LineAndShapeRenderer]
  renderer.setDefaultStroke(BasicStroke(5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
  renderer.setAutoPopulateSeriesStroke(false)


  val file = dataDir / f"${timestamp}_${benchmarkName}.png"

  ChartUtils.saveChartAsPNG(file.toIO, chart, 1700, 1000)
  println(s"Saved plot to $file")

private def makeDatasetFrom(xAxis: Seq[Int], results: ProcessedBenchmarkSeriesResults): CategoryDataset =
  val dataset = DefaultCategoryDataset()

  for
    (algo, algoResults) <- results
    (x, y) <- xAxis.zip(algoResults.map(_.average))
  do
    dataset.addValue(y, algo.toString, x)

  dataset


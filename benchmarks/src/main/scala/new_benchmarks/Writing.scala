package new_benchmarks

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.{XYErrorRenderer, XYLineAndShapeRenderer}
import org.jfree.chart.ui.{HorizontalAlignment, RectangleInsets}
import org.jfree.chart.{ChartUtils, JFreeChart, StandardChartTheme}
import org.jfree.data.xy.{IntervalXYDataset, XYIntervalSeries, XYIntervalSeriesCollection}
import os.{Path, write}

import java.awt.{BasicStroke, Color, Font}
import java.text.SimpleDateFormat
import java.util.Date

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


  val repetitions = results.head._2.head.data.size
  val stringedResults = if repetitions > 1 then
    results.flatMap: (algo, algoResults) =>
      val repetitionColumns = algoResults.head.data.indices.map: repIdx =>
        s"$algo (rep $repIdx)" +: algoResults.map: repRes =>
          repRes.data(repIdx).toString

      val averageColumn = s"$algo (avg)" +: algoResults.map: repRes =>
        repRes.average.toString

      val stdColumn = s"$algo (std)" +: algoResults.map: repRes =>
        repRes.std.get.toString

      repetitionColumns :+ averageColumn :+ stdColumn
  else
    results.map: (algo, algoResults) =>
      val dataColumn = s"$algo" +: algoResults.map: repRes =>
        repRes.average.toString

      dataColumn

  val paramsAndResults = (paramName +: paramRange.map(_.toString)) +: stringedResults

  val table = paramsAndResults.transpose

  val sep = ", "

  val linesToWrite = table.map(_.mkString(sep))
  val strToWrite = linesToWrite.mkString(System.lineSeparator())

  val file = dataDir / f"${timestamp}_${benchmarkName}.csv"

  write(file, strToWrite, createFolders = true)
  println(s"Saved results to $file")

private def createXYErrorLineChart(title: String, xAxisLabel: String, yAxisLabel: String, dataset: IntervalXYDataset, includeError: Boolean): JFreeChart =
  // Adapted from the createXYLineChart method in the ChartFactory class in JFreeChart

  val xAxis = NumberAxis(xAxisLabel)
  xAxis.setAutoRangeIncludesZero(false)
  val yAxis = NumberAxis(yAxisLabel)

  val renderer = XYErrorRenderer()
  renderer.setDefaultLinesVisible(true)
  renderer.setDefaultShapesVisible(true)
  renderer.setDrawXError(false)
  renderer.setDrawYError(includeError)

  val plot = XYPlot(dataset, xAxis, yAxis, renderer)
  plot.setOrientation(PlotOrientation.VERTICAL)

  val chart = JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, true)
  StandardChartTheme("JFree").apply(chart)
  chart

private def saveToPlot(
            benchmarkName: String,
            paramName: String,
            paramRange: Range,
            results: ProcessedBenchmarkSeriesResults,
            dataDir: Path = os.pwd / "benchmarks" / "data",
            timestamp: String
          ): Unit =
  val dataset = makeDatasetFrom(paramRange, results)

  val hasStd = results.head._2.head.std.isDefined

  val chart = createXYErrorLineChart(benchmarkName, paramName, "Time (ms)", dataset, hasStd)

  val titleFont = chart.getTitle.getFont.deriveFont(Font.PLAIN, 45f)
  chart.getTitle.setFont(titleFont)
  chart.getTitle.setPadding(15, 15, 15, 15)

  val legend = chart.getLegend()
  val legendFont = legend.getItemFont.deriveFont(20f)
  legend.setItemFont(legendFont)
  legend.setHorizontalAlignment(HorizontalAlignment.CENTER)
  legend.setItemLabelPadding(RectangleInsets(0, 20, 0, 20))

  val plot = chart.getXYPlot
  val tickFont = plot.getDomainAxis.getTickLabelFont.deriveFont(Font.PLAIN, 20f)
  val labelFont = plot.getDomainAxis.getLabelFont.deriveFont(Font.PLAIN, 25f)
  plot.getDomainAxis.setTickLabelFont(tickFont)
  plot.getRangeAxis.setTickLabelFont(tickFont)
  plot.getDomainAxis.setLabelFont(labelFont)
  plot.getRangeAxis.setLabelFont(labelFont)

  plot.getDomainAxis.setUpperMargin(0.01)
  plot.getDomainAxis.setLowerMargin(0.01)

  plot.setBackgroundPaint(Color.WHITE)
  plot.setRangeGridlinePaint(Color.LIGHT_GRAY)
  plot.setRangeGridlineStroke(BasicStroke(3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

  val renderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
  renderer.setDefaultStroke(BasicStroke(5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
  renderer.setAutoPopulateSeriesStroke(false)


  val file = dataDir / f"${timestamp}_${benchmarkName}.png"

  ChartUtils.saveChartAsPNG(file.toIO, chart, 1700, 1000)
  println(s"Saved plot to $file")

private def makeDatasetFrom(xAxis: Seq[Int], results: ProcessedBenchmarkSeriesResults): IntervalXYDataset =
  val dataset = XYIntervalSeriesCollection()
  val xDoubles = xAxis.map(_.toDouble)

  for (algo, algoResults) <- results do
    val series = XYIntervalSeries(algo.toString)

    for (x, res) <- xDoubles.zip(algoResults) do
      res.std match
        case Some(std) =>
          series.add(
            x,
            x,
            x,
            res.average,
            res.average - std/2,
            res.average + std/2
          )
        case None =>
          series.add(
            x,
            x,
            x,
            res.average,
            0,
            0
          )

    dataset.addSeries(series)

  dataset


package observatory

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Manipulation.{average, deviation, makeGrid}
import observatory.Visualization2.visualizeGrid

import scala.collection.immutable

object Main extends App {

  val generatedSeqs: Seq[(Int, Int, Int)] =
    Seq(0, 1, 2, 3).flatMap(x => triples(x))

  /**
    *
    * @param zoom Zoom level
    * @return Triple tuples
    */
  def triples(zoom: Int): immutable.IndexedSeq[(Int, Int, Int)] = {
    val upper: Double = math.pow(2, zoom) - 1
    for {
      i <- 0 to upper.toInt
      j <- 0 to upper.toInt
    } yield (zoom, i, j)
  }

  // visualizeGrid
  lazy val yearlyData: Iterable[(Int, Iterable[(Location, Double)])] = (1981 to 2015)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  lazy val normalRange: Iterable[(Int, Iterable[(Location, Double)])] = (1975 to 1989)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  lazy val deviationRange: Iterable[(Int, Iterable[(Location, Double)])] = (1975 to 2015)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  // Yearly Data
  yearlyData.foreach({ case (yr, data) =>
    generatedSeqs.foreach({ case (zoom, x, y) =>
      visualizeGrid(makeGrid(data), Colors.temperatures, zoom, x, y)
    })
  })

  // Deviations
  deviationRange.foreach({ case (yr, data) =>
    generatedSeqs.foreach({ case (zoom, x, y) =>
      visualizeGrid(deviation(data, average(normalRange.map(_._2))), Colors.deviations, zoom, x, y)
    })
  })

}

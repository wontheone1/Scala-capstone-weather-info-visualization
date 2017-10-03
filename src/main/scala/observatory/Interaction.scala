package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._
import java.util.concurrent.Executors
import scala.math.{Pi, atan, pow, sinh}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = pow(2.0, zoom + 8)
    val lon = ((x.toDouble / n) % 1.0) * 360.0 - 180.0
    val lat = ((atan(sinh(Pi * (1.0 - 2.0 * y / n))).toDegrees + 90) % 180.0) - 90

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    Visualization.visualizeGeneric(temperatures, 256, 256, 127, colors,
      (x: Int, y: Int) => tileLocation(zoom, (pow(2.0, 8).toInt * x) + x, (pow(2.0, 8).toInt * y) + y))
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 until 4
      y <- 0 until pow(2.0, zoom).toInt
      x <- 0 until pow(2.0, zoom).toInt
    } yield generateImage(year, zoom, x, y, data)
  }

}

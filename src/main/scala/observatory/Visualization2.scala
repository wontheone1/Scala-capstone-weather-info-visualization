package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.math.{pow, floor, ceil}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
      d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) +
      d01 * (1 - x) * y       + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x1 X value of the tile to visualize
    * @param y1 Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x1: Int,
    y1: Int
  ): Image = {
    val imageWidth = 256
    val imageHeight = 256

    val buffer = new Array[Pixel](imageWidth * imageHeight)
    val tasks = for {y2 <- 0 until imageHeight} yield Future {
      for (x2 <- 0 until imageWidth) {
        val pixelLocation = Interaction.tileLocation(zoom, (pow(2.0, 8).toInt * x1) + x2, (pow(2.0, 8).toInt * y1) + y2)
        val latRange = List(floor(pixelLocation.lat).toInt, ceil(pixelLocation.lat).toInt)
        val lonRange = List(floor(pixelLocation.lon).toInt, ceil(pixelLocation.lon).toInt)

        val d = {
          for {
            xPos <- 0 to 1
            yPos <- 0 to 1
          } yield (xPos, yPos) -> grid(latRange(1 - yPos), lonRange(xPos))
        }.toMap

        val xFraction = pixelLocation.lon - lonRange(0)
        val yFraction = latRange(1) - pixelLocation.lat

        val temp = bilinearInterpolation(x=xFraction, y=yFraction, d00=d((0,0)), d01=d((0,1)), d10=d((1,0)), d11=d((1,1)))
        buffer(y2 * imageWidth + x2) = Visualization.colorToPixel(Visualization.interpolateColor(colors, temp), 127)

      }
    }

    Await.result(Future.sequence(tasks), 20.minute)
    Image(imageWidth, imageHeight, buffer)
  }

}

package observatory

import scala.math.{Pi, acos, cos, pow, sin}
import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EARTH_RADIUS = 6371.0
  val P = 3.0
  val MIN_ARC_DISTANCE = 1.0
  val TO_RADIANS = Pi / 180.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    ???
  }

  def dist(x: Location, xi: Location): Double = {
    val deltaLambda = ((x.lon max xi.lon) - (x.lon min xi.lon)) * TO_RADIANS
    val sigma = acos(sin(x.lat * TO_RADIANS) * sin(xi.lat * TO_RADIANS) + cos(x.lat * TO_RADIANS) * cos(xi.lat * TO_RADIANS) * cos(deltaLambda))
    // Convert to arc distance in km
    EARTH_RADIUS * sigma
  }

  def inverseDistanceWeighting(sample: Iterable[(Location, Double)], loc: Location, p: Double) = {
    @tailrec
    def inverseDistanceWeightingRecursion(values: Iterator[(Location, Double)], sumVals: Double, sumWeights: Double): Double = {
      values.next match {
        case (location, temperature) => {
          val arc_distance = dist(loc, location)
          if (arc_distance < MIN_ARC_DISTANCE)
            temperature
          else {
            val weight = 1.0 / pow(arc_distance, p)
            if (values.hasNext)
              inverseDistanceWeightingRecursion(values, sumVals + weight * temperature, sumWeights + weight)
            else
              (sumVals + weight * temperature) / (sumWeights + weight)
          }
        }
      }
    }
    inverseDistanceWeightingRecursion(sample.toIterator, 0.0, 0.0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    def interpolateColor2(sortedPoints: Array[(Double, Color)], value: Double): Color = {
      for (i <- 0 until sortedPoints.length - 1) {
        (sortedPoints(i), sortedPoints(i + 1)) match {
          case ((v1, Color(r1, g1, b1)), (v2, Color(r2, g2, b2))) => {
            if (v1 > value)
              return Color(r1, g1, b1)
            else if (v2 > value) {
              val ratio = (value - v1) / (v2 - v1)
              return Color(
                math.round(r1 + (r2 - r1) * ratio).toInt,
                math.round(g1 + (g2 - g1) * ratio).toInt,
                math.round(b1 + (b2 - b1) * ratio).toInt
              )
            }
          }
        }
      }
      // Value is not within the colourmap.  Return maximum color
      sortedPoints(sortedPoints.length - 1)._2
    }

    val sortedPoints = points.toList.sortWith(_._1 < _._1).toArray
    interpolateColor2(sortedPoints, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val colorMap = colors.toList.sortWith(_._1 < _._1)

    def colorToPixel(c: Color): Pixel = {
      Pixel(c.red, c.green, c.blue, 255)
    }

    val buffer = new Array[Pixel](360 * 180)

    for (y <- 0 until 180) {
      for (x <- 0 until 360) {
        val temp = inverseDistanceWeighting(temperatures, Location(90 - y, x - 180), P)
        buffer(y * 360 + x) = colorToPixel(interpolateColor(colorMap, temp))
      }
    }

    Image(360, 180, buffer)
  }

  val colours: List[(Double, Color)] = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

}


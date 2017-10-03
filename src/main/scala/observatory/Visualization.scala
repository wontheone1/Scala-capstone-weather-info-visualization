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
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}


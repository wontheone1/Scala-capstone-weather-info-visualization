package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val london = Location(51.508530, -0.076132)
  val paris = Location(48.864716, 2.349014)
  val newYork = Location(40.730610, -73.935242)

  val locationGen = for {
    lat <- Gen.choose(-90.0, 90.0)
    lon <- Gen.choose(-180.0, 180.0)
  } yield Location(lat, lon)

  test("dist london->paris") {
    assert(Visualization.dist(london, paris) - 344.0 < 0.2)
  }

  test("dist london->new york") {
    assert(Visualization.dist(london, newYork) - 5585.0 < 0.2)
  }

  test("Symetric inverse distance weighting") {
    val sample: List[(Location, Double)] =
      List((Location(30, 0), 5.0), (Location(0, 30), 5.0), (Location(-30, 0), 5.0), (Location(0, -30), 5.0))
    val loc = Location(0, 0)

    assert(Visualization.idw(sample, loc, Visualization.P) === 5.0)
  }

  test("At point inverse distance weighting") {
    val sample: List[(Location, Double)] =
      List((Location(30, 0), 10.0), (Location(0, 30), 5.0), (Location(-30, 0), 99.0), (Location(0, -30), 20.0))
    val loc = Location(-30, 0)

    assert(Visualization.idw(sample, loc, Visualization.P) === 99.0)
  }

  val colourGen = for {
    red <- Gen.choose(0, 255)
    green <- Gen.choose(0, 255)
    blue <- Gen.choose(0, 255)
  } yield Color(red, green, blue)

  /**
    * Tests for colour linear interpolation
    */
  test("Colour within bounds") {
    val gen = for {
      val1 <- Gen.choose(-50.0, 50.0)
      col1 <- colourGen
      val2 <- Gen.choose(-50.0, 50.0)
      col2 <- colourGen
      value <- Gen.choose(val1 min val2, val1 max val2)
    } yield (List((val1, col1), (val2, col2)), value)

    check(forAll(gen) {
      case (bounds: List[(Double, Color)], value: Double) => {
        val result = Visualization.interpolateColor(bounds.toIterable, value)
        all(
          result.red >= bounds.map(_._2.red).min,
          result.red <= bounds.map(_._2.red).max,
          result.green >= bounds.map(_._2.green).min,
          result.green <= bounds.map(_._2.green).max,
          result.blue >= bounds.map(_._2.blue).min,
          result.blue <= bounds.map(_._2.blue).max
        )
      }
    })
  }
}

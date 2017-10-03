package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class VisualizationTest extends FunSuite with GeneratorDrivenPropertyChecks {

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

  test("dist swap inputs") {
    forAll(locationGen, locationGen) { (loc1: Location, loc2: Location) =>
      assert(Visualization.dist(loc1, loc2) === Visualization.dist(loc2, loc1))
    }
  }
  test("dist london->new york") {
    assert(Visualization.dist(london, newYork) - 5585.0 < 0.2)
  }

  test("Symetric inverse distance weighting") {
    val sample: List[(Location, Double)] =
      List((Location(30, 0), 5.0), (Location(0, 30), 5.0), (Location(-30, 0), 5.0), (Location(0, -30), 5.0))
    val loc = Location(0, 0)

    assert(Visualization.inverseDistanceWeighting(sample, loc, Visualization.P) === 5.0)
  }

  test("At point inverse distance weighting") {
    val sample: List[(Location, Double)] =
      List((Location(30, 0), 10.0), (Location(0, 30), 5.0), (Location(-30, 0), 99.0), (Location(0, -30), 20.0))
    val loc = Location(-30, 0)

    assert(Visualization.inverseDistanceWeighting(sample, loc, Visualization.P) === 99.0)
  }
}

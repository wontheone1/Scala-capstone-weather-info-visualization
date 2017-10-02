package observatory

/**
  * Created by wseobseo on 02/10/2017.
  */
case class StationKey(stn: Option[Int], wban: Option[Int])

case class TempsLine(key: StationKey, month: Int, day: Int, temp: Double)

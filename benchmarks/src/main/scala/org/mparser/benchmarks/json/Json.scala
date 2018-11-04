package org.mparser.benchmarks.json

/**
  * @author Evgenii Kiiski 
  */
sealed trait Json

object Json {

  final case class JString(value: String) extends Json

  final case class JBoolean(value: Boolean) extends Json

  final case class JNumber(value: BigDecimal) extends Json

  final case object JNull extends Json

  final case class JObject(value: Map[String, Json]) extends Json

  final case class JArray(value: Seq[Json]) extends Json

}
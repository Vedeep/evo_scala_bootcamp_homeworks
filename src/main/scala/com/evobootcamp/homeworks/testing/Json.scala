package com.evobootcamp.homeworks.testing

import atto.parser.character.char
import atto.parser.text.{string, stringLiteral}
import atto.parser.combinator.endOfInput
import atto.parser.numeric.double
import atto.syntax.parser._
import atto.Parser

object Json {
  sealed trait Json
  case object JNull extends Json
  final case class JBoolean(value: Boolean) extends Json
  final case class JNumber(value: Double) extends Json
  final case class JString(value: String) extends Json
  final case class JArray(value: Vector[Json]) extends Json
  final case class JObject(value: Map[String, Json]) extends Json

  object Parser {
    val jNull: Parser[JNull.type] =
      string("null") >| JNull
    val jBoolean: Parser[JBoolean] =
      (string("true") >| true | string("false") >| false) -| JBoolean
    val jNumber: Parser[JNumber] =
      double -| JNumber
    val jString: Parser[JString] =
      stringLiteral -| JString
    lazy val jArray: Parser[JArray] =
      (char('[') ~> json.sepBy(char(',')) <~ char(']')) -| { l =>
        JArray(l.toVector)
      }

    lazy val jObject: Parser[JObject] =
      (char('{') ~> ((stringLiteral <~ char(':')) ~ json).sepBy(char(',')) <~ char('}')) -| { l =>
        JObject(l.toMap)
      }
    lazy val json: Parser[Json] =
      jNull | jBoolean | jNumber | jString | jArray | jObject
    lazy val jsonOnly: Parser[Json] =
      json <~ endOfInput
  }

  def parse(s: String): Option[Json] =
    Parser.jsonOnly.parseOnly(s).option

  def print(json: Json): String = json match {
    case JNull => "null"
    case JBoolean(v) => v.toString
    case JNumber(v) => v.toString
    case JString(v) => s""""$v""""
    case JArray(v) => s"[${v.map(print).mkString(",")}]"
    case JObject(v) => s"{${v.toList.map(a => s""""${a._1}":${print(a._2)}""").mkString(",")}}"
  }
}

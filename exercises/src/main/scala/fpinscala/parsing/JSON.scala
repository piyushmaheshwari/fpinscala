package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser [Err, Parser[+_]] (P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    def keyval = escapedQuoted ** (":" *> value)

    def lit = double.map(JNumber(_))

  }

}

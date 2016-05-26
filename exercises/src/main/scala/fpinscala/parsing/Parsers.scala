package fpinscala.parsing

import java.util.regex.Pattern

import language.higherKinds
import language.implicitConversions
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>
  // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  implicit def operators[A](p: Parser[A]) = ParserOps(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  implicit def whitespace: Parser[String] = "\\s*".r

  def digits: Parser[String] = "\\d+".r

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString.map(_.toDouble)

  def eof: Parser[String] =
    regex("\\z".r)

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(p, p2)((a, b) => b)

  def skipR[B](p: Parser[B], p2: => Parser[Any]): Parser[B] =
    map2(p, p2)((a, b) => a)

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] = token(quoted) //handle escaping later

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def many[A](a: Parser[A]): Parser[List[A]] =
    map2(a, many(a))(_ :: _) or succeed(List())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(aa => succeed(f(aa)))

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def many1_2[A](a: Parser[A]): Parser[List[A]] =
    map(product(a, many(a)))(t => (t._1 :: t._2))

  def many1[A](a: Parser[A]): Parser[List[A]] = map2(a, many(a))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def contextSensitiveParser: Parser[Int] =
    ("[1-9]+".r flatMap (x => listOfN(x.toInt, char('a')))).map(_.size)

  def contextSensitiveParser_1: Parser[Int] =
    for {
      r <- "[1-9]+".r
      digit = r.toInt
      _ <- listOfN(digit, char('a'))
    } yield (digit)


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B) = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] =
      self.skipR(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] =
      self.skipL(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)
  }

  object Laws {
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class  ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] = latest map (_._1)


}
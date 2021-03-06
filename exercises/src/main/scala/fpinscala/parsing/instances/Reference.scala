package fpinscala.parsing.instances

import fpinscala.parsing.{Location, ParseError, Parsers}
import ReferenceTypes.{Failure, ParseState, Parser, Success}

import scala.util.matching.Regex

object ReferenceTypes {
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))

    def input: String = loc.input.substring(loc.offset)

    def slice (n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  trait Result[+A] {

    def extract: Either[ParseError, A] = this match {
      case Failure (e, _) => Left (e)
      case Success (a, _) => Right (a)
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, _) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, c) => Success(a, n + c)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

}


object Reference extends Parsers[Parser] {

  def run[A](p: Parser[A])(s: String): Either[ParseError, A] = {
    val input = ParseState(Location(s, 0))
    p (input).extract
  }

  override def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def string(w: String): Parser[String] = {
    val msg = "'" + msg + "'"
    s => {
      val i = firstNonMatchingIndex(s.loc.input, w, s.loc.offset)
      if (i == -1)
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  def firstNonMatchingIndex(s1: String, s2: String, offset: Int) = {
    0 //todo later
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => r.findPrefixOf(s.input) match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  def or[A](p: Parser[A], p2: Parser[A]): Parser[A] =
    s => p(s) match {
      case Failure(e, false) => p2(s)
      case r => r
    }

  def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0).advanceSuccess(n)
      case e: Failure => e
    }

  def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  def slice[A](p: Parser[A]): Parser[String] = s => p (s) match {
    case Success (_, n) => Success (s.slice(n), n)
    case f : Failure => f
  }

  override def many[A](p: Parser[A]): Parser[List[A]] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    (s) => p(s).mapError(_.push(s.loc, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))
}

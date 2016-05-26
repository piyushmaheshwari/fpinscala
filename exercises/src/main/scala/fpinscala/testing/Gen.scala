package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import com.sun.javafx.webkit.Accessor.PageAccessor

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

sealed trait Result {
  def isFalsified : Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified (failure: FailedCase, success: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop (run: (MaxSize, TestCases, RNG) => Result) {
  def && (p : Prop): Prop = Prop {
    (max, n, rng) => this.run (max, n, rng) match {
      case Passed => p.run (max, n, rng)
      case a => a
    }
  }

  def || (p : Prop): Prop = Prop {
    (max, n, rng) => this.run (max, n, rng) match {
      case Passed => Passed
      case a => p.run (max, n, rng)
    }
  }
}

case class Gen [+A] (sample : State[RNG, A]) {

  def flatMap [B] (f : A => Gen[B]): Gen [B] =
    Gen (State[RNG, B](s => {
      val (v, next) = sample.run (s)
      f (v).sample.run(next)
    }))

  def map [B] (f : A => B): Gen [B] = this flatMap(a => Gen.unit(f(a)))

  def listOfN (size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN (size: Gen[Int]): Gen[List[A]] =
    size.flatMap(a => Gen.listOfN(a, this))

  def unsized: SGen [A] = SGen ( s => this)
}

case class SGen [+A] (g: Int => Gen [A]) {
  def apply (n : Int): Gen [A] = g(n)

  def flatMap [B] (f : A => Gen[B]): SGen [B] =
    SGen (g andThen(_ flatMap(f)))

  def map [B] (f : A => B): SGen [B] = SGen (g andThen (_ map f))


}

object SGen {
  def listOf [A] (g : Gen [A]) : SGen [List[A]] = SGen (n => g.listOfN(n))

  def listOf1 [A] (g : Gen [A]): SGen [List[A]] = SGen (n => g.listOfN(n max 1))
}

object Gen {
  import RNG._

  def unit[A](a: => A): Gen[A] = Gen (State (RNG.unit(a)))

  def choose (start: Int, stopExclusive: Int): Gen[Int] =
    Gen (State (nonNegativeInt).map(a => start + a % (stopExclusive - start)))

  def listOfN [A] (n: Int, g: Gen [A]): Gen[List[A]] =
    Gen (State (sequence(List.fill(n)(g.sample.run))))

  def union [A] (g1: Gen[A], g2: Gen[A]): Gen [A] =
    Gen (State(s => {
      Gen.choose(0,2).sample.flatMap(v =>
        if (v == 0) g1.sample else g2.sample).run (s)
    }))

  def weighted [A] (g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen [A] = {
    val p = g1._2 / (g1._2 + g2._2)
    Gen (State (RNG.double)).flatMap (a => if (a % 100 < p) g1._1 else g2._1)
  }

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
}


object GenTest {
  def main(args: Array[String]): Unit = {

    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(smallInt)
  }
}
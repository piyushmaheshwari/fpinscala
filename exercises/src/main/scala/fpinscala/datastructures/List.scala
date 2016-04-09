package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception ("tail of a empty list")
    case Cons (_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Exception ("set head on an empty list")
    case Cons (_, t) => Cons (h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons (h, t) if (n > 0) => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons (h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception ("init on an empty list")
    case Cons (h, Nil) => Nil
    case Cons (h, t) => Cons (h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((x, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =  l match {
    case Nil => z
    case Cons (h, t) => foldLeft(t, f (z, h)) (f)
  }

  def sum3(l : List[Int]) = foldLeft(l, 0)(_ + _)
  def product3 (l : List[Double]) = foldLeft(l, 1.0) (_ * _)
  def length2 [A] (l : List [A]): Int = foldLeft(l, 0) ((a,b) => (a + 1))

  def reverse [A] (l : List[A]): List[A] = foldLeft(l, Nil: List[A])((acc , h) => Cons (h, acc))

  def foldRightViaFoldLeft[A, B] (l : List[A], z: B) (f : (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f (a, b))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a: A, g) => b => g(f(b,a)))(z)

  def appendViaFoldRight [A] (l : List[A], r: List[A]) = foldRight(l, r) ((a, b) => Cons (a,b))

  def concat[A] (l : List[List[A]]): List [A] = foldRight(l, Nil: List[A])(appendViaFoldRight)

  def add1 (l : List[Int]) = foldRight(l, Nil: List[Int]) ((a, b) => Cons (a + 1, b))

  def doubleToString (l : List[Double]) = foldRight (l, Nil: List[String]) ((a,b) => Cons (a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight (l, Nil: List[B]) ((a, t) => Cons (f(a), t))

  def map_2 [A, B] (l : List[A])(f: A => B): List [B] = {
    val buf = new collection.mutable.ListBuffer[B]
    @annotation.tailrec
    def go (ll : List[A]): Unit = ll match {
      case Nil => ()
      case Cons (h, t) => buf += f (h); go (t);
    }
    go (l)
    List (buf.toList: _*)
  }

  def filter [A] (l : List[A])(p : A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => if (p(a)) Cons (a, b) else b)

  def flatMap [A, B] (l : List[A])(f : A => List[B]): List [B] =
    foldRight(l, Nil: List[B]) ((a,b) => appendViaFoldRight(f (a), b))

  def filterViaFlatMap [A] (l : List[A]) (p: A => Boolean): List[A] =
    flatMap(l) (a => if (p(a)) List (a) else Nil)

  def addLists (a : List[Int], b: List[Int]): List[Int] = {
    val buff = new collection.mutable.ListBuffer[Int]
    def go (first: List[Int], second: List[Int]): Unit = first match {
      case Nil => ()
      case Cons (h, t) => second match {
        case Nil => ()
        case Cons (hh, tt) => buff += (h + hh); go  (t, tt);
      }
    }

    go (a, b)
    List (buff.toList: _*)
  }

  def zipWith [A,B,C](a : List[A], b: List[B])(f : (A, B) => C): List[C] = (a,b) match {
    case (Cons (h1, t1), Cons (h2, t2)) => Cons (f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  @annotation.tailrec
  def startsWith[A](first: List[A], second: List[A]): Boolean = (first, second) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(h, t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
  }
}



object Test {
  def main(args: Array[String]): Unit = {
    println (List.hasSubsequence(List (1,2,3,4), List (3)))
    println (List.hasSubsequence(List (1,2,3,4), List (1)))
    println (List.hasSubsequence(List (1,2,3,4), List (2,3)))
    println (List.hasSubsequence(List (1,2,3,4), List (2,4)))
  }
}
package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListEfficient: List [A] = {
    @annotation.tailrec
    def go [A] (l : Stream[A], acc: List[A]): List[A] = l match {
      case Empty => acc
      case Cons (h, t) => go (t(), h ():: acc)
    }
    go (this, Nil).reverse
  }



  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (n, Cons (h, t)) if (n > 0) => cons(h(), t().take(n - 1))
    case (_,_) => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons (h, t) if (p (h())) => cons (h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons (h, t) => if (!p(h ())) false else t().forAll(p)
    case Empty => true
  }

  def forAllViaFoldRight (p: A => Boolean): Boolean =
      foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFoldRight (p: A => Boolean): Stream [A] =
    foldRight(empty [A])((a,b) => if (p(a)) cons(a,b) else empty)

  def headOption: Option[A] = foldRight(None: Option[A])((a,b) => Some (a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons (f (a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) ((a,b) => if (f(a)) cons (a,b) else b)

  def append[AA >: A](other: => Stream[AA]): Stream[AA] = foldRight(other)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.map(f).foldRight(empty[B])((a,b) => a.append(b))

  def mapViaUnfold [B] (f : A => B): Stream [B] =
    unfold(this)(_ match {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    })

  def takeViaUnfold (n : Int): Stream [A] = unfold ((this, n)) (_ match {
    case (Cons (h, t), n) if (n > 1) => Some (h(), (t (), n -1 ))
    case (Cons (h, _), 1) => Some (h(), (empty,0))
    case _ => None
  })

  def takeWhileViaUnfold (f : A => Boolean): Stream [A] =
    unfold(this)(_ match {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case Empty => None
    })

  def zipWith[B,C] (b : Stream [B])(f : (A, B) => C): Stream [C] =
    unfold(this, b)(_ match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    })

  def zipAll [B] (s2: Stream[B]): Stream [(Option[A], Option[B])] =
    unfold ((this, s2)){
      case (Empty, Empty) => None
      case (Cons (h, t), Empty) => Some ((Some(h()), None), (t (), Empty))
      case (Empty, Cons (h,t)) => Some ((None, Some (h())), (Empty, t()))
      case (Cons (h1, t1), Cons (h2, t2)) => Some ((Some (h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile (_._2.isDefined).forAll({
      case (a,b) => a == b
    })

  def tails: Stream[Stream[A]] = unfold(this) (s => s match {
    case Cons (h,t) => Some (s, t())
    case Empty => None
  }) append Stream (empty)


  def scanRight [B] (z: B) (f : (A, => B) => B) : Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
        lazy val x = b
        val r = f (a, x._1)
        (r, cons(r, x._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = cons (n, from (n + 1))

  val fibs = {
    def go (a: Int, b: Int): Stream[Int] = cons (a, go (b, a + b))
    go (0,1)
  }

  def constant [A] (a : A): Stream [A] = cons (a, constant(a))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some ((a,s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  def constantViaUnfold [A] (a : A): Stream [A] = unfold(a)(_ => Some (a, a))
  val onesViaUnfold: Stream [Int] = constantViaUnfold(1)

  def fromViaUnfold (n : Int): Stream [Int] = unfold(n)(a => Some (a, a + 1))

  val fibsViaUnfold = unfold((0,1)) (s => Some (s._1, (s._2, s._1 + s._2)))
}

object StreamTest {
  def main(args: Array[String]): Unit = {
    val y = Stream (1,2,3)
    println (y.scanRight(0)(_ + _).toList)
  }
}
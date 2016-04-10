package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A] (t : Tree[A]): Int = t match {
    case Leaf (_) => 1
    case Branch (left, right)  => 1 + size(left) + size (right)
  }

  def maximum (t : Tree[Int]): Int = t match {
    case Leaf (v) => v
    case Branch (left, right) => Math.max(maximum(left), maximum(left))
  }

  def depth [A] (t: Tree[A]): Int = t match {
    case Leaf (_) => 0
    case Branch (l, r) => 1 + Math.max(depth(l), depth (r))
  }

  def map [A, B] (t: Tree [A])(f : A => B): Tree [B] = t match {
    case Leaf (v) => Leaf (f(v))
    case Branch (l, r) => Branch (map(l)(f), map (r)(f))
  }

  def fold[A, B] (t : Tree[A])(lf: A => B)(bf : (B,B) => B): B = t match {
    case Leaf (v) => lf (v)
    case Branch (l, r) => bf (fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def sizeViaFold[A] (t : Tree[A]): Int = fold(t)(a => 1)(_ + _ + 1)

  def maximumViaFold (t : Tree[Int]): Int = fold (t)(a => a)((x,y)=> Math.max(x,y))

  def depthViaFold[A] (t : Tree[A]): Int = fold (t)(a => 0)((x,y) => 1 + Math.max(x,y))

  def mapViaFold [A, B] (t : Tree[A])(f : A => B): Tree[B] =
    fold(t)(b => Leaf(f(b)): Tree[B])(Branch(_, _))
}

object TreeTest {
  def main(args: Array[String]): Unit = {
    val left = Leaf(2)
    val right = new Branch(Leaf (3), Leaf (4))
    val tree = new Branch(left, right)
    println (Tree.size(tree))
    println (Tree.maximum(tree))
    println (Tree.depth(tree))
  }
}
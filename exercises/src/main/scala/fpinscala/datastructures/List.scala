package fpinscala.datastructures


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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

  def sum2(l: List[Int]) =
    foldLeft(l, 0)((x, y) => x + y)

  def product2(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`, see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l
  else l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def initAcc(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => acc
        case Cons(x, xs) => initAcc(xs, Cons(x, acc))
      }
    }

    reverse(initAcc(l, Nil))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())(append)

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  def zipWith[A, B, C](l: List[A], m: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def go(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x, y), acc))
    }
    reverse(go(l, m, Nil))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
    case (_, Nil) => true
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case _ if startsWith(l, sub) => true
    case Cons(_, xs) => hasSubsequence(xs, sub)
    case _ => false
  }
}
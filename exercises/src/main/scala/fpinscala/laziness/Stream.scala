package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

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

  def take(n: Int): Stream[A] = {
    def go(n: Int, s: Stream[A], acc: Stream[A]): Stream[A] = {
      if (n == 0) acc
      else s match {
        case Cons(h, t) => Cons(h, () => go(n-1, t(), acc))
        case _ => acc
      }
    }
    go(n, this, Stream[A]())
  }

  def drop(n: Int): Stream[A] = {
    def go(n: Int, s: Stream[A]): Stream[A] = {
      if (n == 0) s
      else s match {
        case Cons(_, t) => go(n-1, t())
        case _ => s
      }
    }
    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { (h,t) =>
      if (p(h)) cons(h,t)
      else empty[A]
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")


  // Additional

  def toList: List[A] = foldRight(List[A]()) (_ :: _)

  def headOption: Option[A] = foldRight(None: Option[A])((a,_) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](s: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => s(h).append(t))
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

  val ones: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {

  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def nextFib(n: Int, n1: Int): Stream[Int] = cons(n, nextFib(n1, n+n1))
    nextFib(0, 1)
  }
}
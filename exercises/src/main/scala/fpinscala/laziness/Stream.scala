package fpinscala.laziness

import Stream._
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
      if (p(h)) Cons(() => h, () => t)
      else empty[A]
    }
  }

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")


  // Additional

  def toList: List[A] = foldRight(List[A]()) (_ :: _)
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

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
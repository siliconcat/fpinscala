package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }



  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = Either.lift2(f)(this,b)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def lift2[E, A, B, C](f: (A, B) => C): (Either[E, A], Either[E, B]) => Either[E, C] = {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(x), _) => Left(x)
    case (_, Left(x)) => Left(x)
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es.foldRight[Either[E, List[B]]](Right(Nil))((a, bs) => f(a).map2(bs)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)
}
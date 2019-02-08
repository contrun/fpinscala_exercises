package chap04

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None    => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None    => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None    => None
      case Some(a) => f(a)
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _    => this
    }
  }

  def filter(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) match {
      case None => None
      case Some(m) => {
        mean(xs.map(x => (math.pow(x - m, 2))))
      }
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a match {
      case None => None
      case Some(av) => {
        b match {
          case None     => None
          case Some(bv) => Some(f(av, bv))
        }
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    try {
      Some(a.map(x => {
        x match {
          case None    => throw new Exception("None found")
          case Some(m) => m
        }
      }))
    } catch { case e: Exception => None }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    try {
      Some(a.map(x => {
        f(x) match {
          case None    => throw new Exception("None found")
          case Some(m) => m
        }
      }))
    } catch { case e: Exception => None }
  }

  def main5(args: Array[String]) {
    def f(a: Int): Option[Int] = {
      a match {
        case 0 => None
        case _ => Some(a)
      }
    }
    assert(None == traverse[Int, Int](List(1, 0))(f))
    assert(Some(List(1, 2)) == traverse(List(1, 2))(f))
  }

  def main4(args: Array[String]) {
    assert(None == sequence(List(Some(1), None)))
    assert(Some(List(1, 2)) == sequence(List(Some(1), Some(2))))
  }

  def main3(args: Array[String]) {
    assert(Some(2) == map2(Some(1), Some(2))(_ * _))
    assert(None == map2(Some(1), None)((x: Int, y: Int) => x * y))
  }

  def main2(args: Array[String]) {
    assert(variance(Array(1.0, 1.0)).getOrElse(1000.0) <= 0.1)
    assert(variance(Array(1.0, 2.0, 3.0)).getOrElse(1000.0) <= 1.0)
    assert(variance(Array[Double]()) == None)
  }

  def main1(args: Array[String]) {
    assert(Some(2) == Some(1).map(_ + 1))
    assert(2 == None.getOrElse(2))
    assert(3 == Some(3).getOrElse(2))
    assert(Some(2) == None.orElse(Some(2)))
    assert(Some(3) == Some(3).orElse(Some(2)))
    assert(Some(3) == Some(2).flatMap(x => Some(x + 1)))
  }
}

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e)  => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => {
        b match {
          case Left(e)  => Left(e)
          case Right(b) => Right(f(a, b))
        }
      }
    }
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] = {
    Right(es.map(x => {
      f(x) match {
        case Left(e)  => return Left(e)
        case Right(m) => m
      }
    }))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    Right(es.map(x => {
      x match {
        case Left(e)  => return Left(e)
        case Right(m) => m
      }
    }))
  }

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

  def main8(args: Array[String]) {
    // change it to something like Either[String, Either[String, A]]?
  }

  def main7(args: Array[String]) {
    assert(Left("error") == sequence(List(Right(1), Left("error"))))
    assert(Right(List(1, 2)) == sequence(List(Right(1), Right(2))))

    def f(a: Int): Either[String, Int] = {
      a match {
        case 0 => Left("error")
        case _ => Right(a)
      }
    }
    assert(Left("error") == traverse[String, Int, Int](List(1, 0))(f))
    assert(Right(List(1, 2)) == traverse(List(1, 2))(f))

  }
  def main6(args: Array[String]) {
    assert(Right(2) == Right(1).map2(Right(2))(_ * _))
    assert(
      Left("error") == Right(1).map2(Left("error"))((x: Int, y: Int) => x * y))

  }
}

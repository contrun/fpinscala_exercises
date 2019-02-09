package chap05

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    if (n == 0) {
      return Empty
    }
    this match {
      case Empty      => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def take2(n: Int): Stream[A] = {
    unfold[A, (Int, Stream[A])]((n, this))(x => {
      x._2 match {
        case Empty => None
        case Cons(h, t) =>
          if (x._1 == 0) {
            None
          } else {
            Some((h(), (x._1 - 1, t())))
          }
      }
    })
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) {
      return this
    }
    this match {
      case Empty      => Empty
      case Cons(h, t) => t().drop(n - 1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (p(h())) { Cons(h, () => t().takeWhile(p)) } else {
          t().takeWhile(p)
        }
      }
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    this.foldRight[Stream[A]](Stream())((a, b) =>
      if (p(a)) { Cons(() => a, () => b) } else { b })
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfoldWithFilter(this)(x => {
      x match {
        case Empty => None
        case Cons(h, t) => {
          if (p(h())) { Some(Some(h()), t()) } else { Some(None, t()) }
        }
      }
    })
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = {
    def append[A](hd: => Stream[A], tl: A): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      hd match {
        case Empty => Cons(() => tl, () => Empty)
        case Cons(h, t) => {
          lazy val lh = h
          Cons(lh, () => append(t(), tl))
        }
      }
    }
    val reversed = foldRight[Stream[A]](Empty)((a, b) => append(b, a))
    reversed.foldRight[Option[A]](None)((a, b) =>
      b match {
        case None => Some(a)
        case x    => x
    })
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    // this match {
    //   case Empty => Empty
    //   case Cons(h, t) => {
    //     Cons(() => f(h()), () => t().map()(f))
    //   }
    // }
    foldRight[Stream[B]](Empty)((a, b) => cons(f(a), b))
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this)(x => {
      x match {
        case Empty      => None
        case Cons(h, t) => Some(f(h()), t())
      }
    })
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a, b) => if (f(a)) { cons(a, b) } else b)
  }

  def append[B >: A](tl: Stream[B]): Stream[B] = {
    // this match {
    //   case Empty => tl
    //   case Cons(h, t) => {
    //     cons(h(), t().append(tl))
    //   }
    // }
    foldRight[Stream[B]](tl)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]) = {
    foldRight[Stream[B]](Empty)((a, b) => f(a).append(b))
  }

  def zip[B](bs: Stream[B]): Stream[(A, B)] = {
    unfold((this, bs))(x => {
      x._2 match {
        case Empty => None
        case Cons(bh, bt) => {
          x._1 match {
            case Empty        => None
            case Cons(ah, at) => Some((ah(), bh()), (at(), bt()))
          }
        }
      }
    })
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, bs))(x => {
      x._2 match {
        case Empty => {
          x._1 match {
            case Empty        => None
            case Cons(ah, at) => Some((Some(ah()), None), (at(), Empty))
          }
        }
        case Cons(bh, bt) => {
          x._1 match {
            case Empty        => Some((None, Some(bh())), (Empty, bt()))
            case Cons(ah, at) => Some((Some(ah()), Some(bh())), (at(), bt()))
          }
        }
      }
    })
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    this.zip(bs).map(x => f(x._1, x._2))
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    this.zipWith(s)(_ == _).forAll(x => x)
  }

  def toList: List[A] = {
    this match {
      case Empty      => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)(x => {
      x match {
        case Empty      => None
        case Cons(h, t) => Some(x, t())
      }
    })
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
    // this match {
    //   case Empty => Stream(z)
    //   case Cons(h, t) => {
    //     val r = t().scanRight(z)(f)
    //     r match {
    //       case Empty        => Stream(z)
    //       case Cons(rh, rt) => cons(f(h(), rh()), r)
    //     }
    //   }
    // }
    foldRight[Stream[B]](Stream(z))((a, sb) =>
      sb match {
        case Empty      => Stream(z)
        case Cons(h, t) => cons(f(a, h()), sb)
    })
  }
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
  def from(a: Int): Stream[Int] = {
    lazy val as: Stream[Int] = Stream.cons(a, as.map(_ + 1))
    as
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None         => Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def unfoldWithFilter[A, S](z: S)(
      f: S => Option[(Option[A], S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a, s)) => {
        a match {
          case None     => unfoldWithFilter(s)(f)
          case Some(av) => Stream.cons(av, unfoldWithFilter(s)(f))
        }
      }
    }
  }

  def myThunk(n: Int): () => Int = { () =>
    {
      println(f"called myThunk with n=${n}")
      n
    }
  }
  val myStream =
    Cons(myThunk(3),
         () => Cons(myThunk(2), () => Cons(myThunk(1), () => Stream())))

  def main16(args: Array[String]) {
    // It seems that we can not construct scanRight with unfold, unfold basically is foldLeft.
    // We have to flip the list in order to construct scanRight. Which can not be done in linear time.
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }

  def main15(args: Array[String]) {
    assert(
      Stream(1, 2).tails.toList
        .map(_.toList) == Stream(Stream(1, 2), Stream(2)).toList
        .map(_.toList))
  }

  def main14(args: Array[String]) {
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  }

  def main13(args: Array[String]) {
    assert(Stream(3).toList == Stream(2, 3, 1, 2).takeWhile3(_ > 2).toList)
    assert(Stream(3, 2).toList == myStream.takeWhile3(_ > 1).toList)
    assert(from(2).map2(_ + 1).exists(_ == 1000))
    assert(from(2).map2(_ + 1).take2(2).toList == List(3, 4))
    assert(
      Stream(1, 2, 3).zip(Stream(2, 3, 4)).toList == List((1, 2),
                                                          (2, 3),
                                                          (3, 4)))
    assert(
      Stream(1, 2, 3).zipAll(Stream(2, 3)).toList == List((Some(1), Some(2)),
                                                          (Some(2), Some(3)),
                                                          (Some(3), None)))
    assert(
      Stream(1, 2, 3).zipWith(Stream(2, 3, 4))(_ + _).toList == List(3, 5, 7))

  }

  def main12(args: Array[String]) {
    def constant[A](a: A): Stream[A] = {
      unfold[A, A](a)(x => Some((x, x)))
    }
    def from(n: Int): Stream[Int] = {
      unfold[Int, Int](n)(x => Some((x, x + 1)))
    }
    def genfib(a: Int, b: Int): Stream[Int] = {
      unfold[Int, (Int, Int)]((a, b))(x => Some((x._1, (x._2, x._1 + x._2))))
    }
    assert(constant(2).map(_ + 1).exists(_ % 3 == 0))
    assert(constant(2).map(_ + 1).take(2).toList == List(3, 3))
    assert(from(2).map(_ + 1).exists(_ == 1000))
    assert(from(2).map(_ + 1).take(2).toList == List(3, 4))
    assert(genfib(0, 1).take(5).toList == List(0, 1, 1, 2, 3))
  }

  def main11(args: Array[String]) {
    assert(unfold[Int, Int](2)(x => Some((x, x))).map(_ + 1).exists(_ % 3 == 0))
    assert(
      unfold[Int, Int](2)(x => Some((x, x))).map(_ + 1).take(2).toList == List(
        3,
        3))
  }

  def main10(args: Array[String]) {
    def zip[A, B](as: Stream[A], bs: Stream[B]): Stream[(A, B)] = {
      as match {
        case Empty => Empty
        case Cons(ah, at) => {
          bs match {
            case Empty => Empty
            case Cons(bh, bt) => {
              cons((ah(), bh()), zip(at(), bt()))
            }
          }
        }
      }
    }

    def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(
        f: (A, B) => C): Stream[C] = {
      zip(as, bs).map(x => f(x._1, x._2))
    }

    def genfib(a: Int, b: Int): Stream[Int] = {
      // lazy is a must
      // see https://stackoverflow.com/questions/31609005/scala-forward-reference-extends-over-definition?rq=1
      lazy val as: Stream[Int] =
        Stream.cons(a, Stream.cons(b, zipWith(as, as.drop(1))(_ + _)))
      as
    }
    assert(
      zip(Stream(1, 2, 3), Stream(2, 3, 4)).toList == List((1, 2),
                                                           (2, 3),
                                                           (3, 4)))
    assert(
      zipWith(Stream(1, 2, 3), Stream(2, 3, 4))(_ + _).toList == List(3, 5, 7))
    assert(genfib(0, 1).take(5).toList == List(0, 1, 1, 2, 3))
  }

  def main9(args: Array[String]) {
    assert(from(2).map(_ + 1).exists(_ == 1000))
    assert(from(2).map(_ + 1).take(2).toList == List(3, 4))
  }

  def main8(args: Array[String]) {
    def constant[A](a: A): Stream[A] = {
      // lazy is a must
      // see https://stackoverflow.com/questions/31609005/scala-forward-reference-extends-over-definition?rq=1
      lazy val as: Stream[A] = Stream.cons(a, as)
      as
    }
    assert(ones.map(_ + 1).exists(_ % 2 == 0))
    assert(constant(2).map(_ + 1).exists(_ % 3 == 0))
    assert(constant(2).map(_ + 1).take(2).toList == List(3, 3))
  }

  def main7(args: Array[String]) {
    assert(
      Stream(2, 1, 0).toList ==
        myStream
          .map(x => {
            println(f"running mapping with x=${x}")
            x - 1
          })
          .toList)
    assert(Stream(3, 2).toList == myStream.filter(_ > 1).toList)
    assert(Stream(3, 2, 1, 0).toList == myStream.append(Stream(0)).toList)
    assert(
      Stream(3, 3, 2, 2, 1,
        1).toList == myStream.flatMap(x => Stream(x, x)).toList)
  }
  def main6(args: Array[String]) {
    assert(myStream.headOption == Some(3))
    assert(Stream().headOption == None)
  }

  def main5(args: Array[String]) {
    assert(Stream(3).toList == Stream(2, 3, 1, 2).takeWhile2(_ > 2).toList)
    assert(Stream(3, 2).toList == myStream.takeWhile2(_ > 1).toList)
  }

  def main4(args: Array[String]) {
    assert(!myStream.forAll(_ > 2))
    assert(myStream.forAll(_ > 0))
  }

  def main3(args: Array[String]) {
    assert(Stream(3, 2).toList == myStream.takeWhile2(_ > 1).toList)
    assert(Stream(3).toList == Stream(2, 3, 1, 2).takeWhile(_ > 2).toList)
  }

  def main2(args: Array[String]) {
    assert(Stream(3, 2).toList == myStream.take(2).toList)
    assert(Stream(1).toList == myStream.drop(2).toList)
    assert(Stream(1, 2).toList == Stream(1, 2, 3).take(2).toList)
    assert(Stream(1, 2).toList == Stream(2, 3, 1, 2).drop(2).toList)
  }

  def main1(args: Array[String]) {
    assert(List(1, 2) == apply(1, 2).toList)
  }
}

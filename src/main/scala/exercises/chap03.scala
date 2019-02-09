package chap03

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil        => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil           => Nil
      case Cons(head, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (_, 0)   => l
      case (Nil, _) => Nil
      case (_, _)   => drop(tail(l), n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h)) {
          dropWhile(t, f)
        } else {
          Cons(h, dropWhile(t, f))
        }
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        t match {
          case Nil => Nil
          case _   => Cons(h, init(t))
        }
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a, b) => b + 1)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = ???

  def main1(args: Array[String]) {
    println(x)
  }

  def main2(args: Array[String]) {
    println(tail(Nil) == Nil)
    println(tail(Cons(1, Nil)) == Nil)
    println(tail(Cons(1, Cons(2, Nil))) == Cons(2, Nil))
  }

  def main3(args: Array[String]) {
    println(setHead(Nil, 1) == Cons(1, Nil))
    println(setHead(Cons(1, Nil), 2) == Cons(2, Nil))
  }

  def main4(args: Array[String]) {
    println(drop(Nil, 10) == Nil)
    println(drop(Cons(1, Nil), 10) == Nil)
    println(drop(Cons(1, Cons(2, Nil)), 1) == Cons(2, Nil))
  }

  def main5(args: Array[String]) {
    def f(n: Int): Boolean = {
      n >= 3
    }
    println(dropWhile(Nil, f) == Nil)
    println(dropWhile(Cons(4, Nil), f) == Nil)
    println(dropWhile(Cons(2, Nil), f) == Cons(2, Nil))
    println(dropWhile(Cons(2, Cons(4, Nil)), f) == Cons(2, Nil))
  }

  def main6(args: Array[String]) {
    println(init(Nil) == Nil)
    println(init(Cons(1, Nil)) == Nil)
    println(init(Cons(1, Cons(2, Nil))) == Cons(1, Nil))
  }

  def main7(args: Array[String]) {}

  def main8(args: Array[String]) {
    val x = Cons(1, Cons(2, Cons(3, Nil)))
    val y = foldRight(x, Nil: List[Int])(Cons(_, _))
    println(x == y)
  }

  def main9(args: Array[String]) {
    println(length(Nil) == 0)
    println(length(List(1, 3, 3)) == 3)
  }

  def main10(args: Array[String]) {
    def length[A](l: List[A]): Int = {
      foldLeft(l, 0)((a, b) => a + 1)
    }
    println(length(Nil) == 0)
    println(length(List(1, 3, 3)) == 3)
  }

  def main11(args: Array[String]) {
    def length[A](l: List[A]): Int = {
      foldLeft(l, 0)((a, b) => a + 1)
    }
    println(length(Nil) == 0)
    println(length(List(1, 3, 3)) == 3)

    def sum(l: List[Int]): Int = {
      foldLeft(l, 0)((a: Int, b: Int) => a + b)
    }
    println(sum(Nil) == 0)
    println(sum(List(1, 2, 3)) == 6)

    def prod(l: List[Int]): Int = {
      foldLeft[Int, Int](l, 1)((a, b) => a * b)
    }
    println(prod(Nil) == 1)
    println(prod(List(1, 2, 3)) == 6)
  }

  def main12(args: Array[String]) {
    def reverse[A](as: List[A]): List[A] = {
      foldLeft[A, List[A]](as, Nil: List[A])((a, b) => Cons(b, a))
    }
    println(List() == reverse(List()))
    println(List(1, 2, 3) == reverse(List(3, 2, 1)))
  }

  def main13(args: Array[String]) {
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(foldLeft[A, List[A]](as, Nil: List[A])((b, a) => Cons(a, b)), z)(
        (b, a) => f(a, b))
    }
    def sum(ns: List[Int]) =
      foldRight2(ns, 0)((x, y) => x + y)
    println(sum(Nil) == 0)
    println(sum(List(1, 2, 3)) == 6)

    def foldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldRight(
        foldRight[A, List[A]](as, Nil: List[A])((a, b) => append(b, List(a))),
        z)((a, b) => f(a, b))
    }
    def prod(l: List[Int]): Int = {
      foldLeft2[Int, Int](l, 1)((a, b) => a * b)
    }
    println(prod(Nil) == 1)
    println(prod(List(1, 2, 3)) == 6)
  }

  def main14(args: Array[String]) {
    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons(_, _))
    println(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))

    def append2[A](a1: List[A], a2: List[A]): List[A] = {
      def appendToList(a1: List[A], a: A): List[A] =
        a1 match {
          case Nil        => Cons(a, Nil)
          case Cons(h, t) => Cons(h, appendToList(t, a))
        }
      foldLeft(a2, a1)(appendToList)
    }
    println(append2(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  def main15(args: Array[String]) {
    def concat[A](lists: List[List[A]]): List[A] = {
      foldLeft(lists, Nil: List[A])(append(_, _))
    }
    println(
      concat(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6))
  }

  def main16(args: Array[String]) {
    def add1(list: List[Int]): List[Int] = {
      foldRight[Int, List[Int]](list, Nil)((a, b) => Cons((a + 1), b))
    }
    println(add1(List(1, 2, 3)) == List(2, 3, 4))
  }

  def main17(args: Array[String]) {
    def toString(list: List[Int]): List[String] = {
      foldRight(list, Nil: List[String])((a, b) => Cons(a.toString, b))
    }
    println(toString(List(1, 2, 3)) == List("1", "2", "3"))
  }

  def main18(args: Array[String]) {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
    def toString(list: List[Int]): List[String] = {
      map[Int, String](list)(_.toString)
    }
    println(toString(List(1, 2, 3)) == List("1", "2", "3"))
  }

  def main19(args: Array[String]) {
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((a, b) => if (f(a)) { b } else { Cons(a, b) })
    }
    def f(n: Int): Boolean = {
      n >= 3
    }
    println(filter(Nil)(f) == Nil)
    println(filter(Cons(4, Nil))(f) == Nil)
    println(filter(Cons(2, Nil))(f) == Cons(2, Nil))
    println(filter(Cons(2, Cons(4, Nil)))(f) == Cons(2, Nil))
  }

  def main20(args: Array[String]) {
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B])((a, b) => append(f(a), b))
    def toString(list: List[Int]): List[String] = {
      flatMap[Int, String](list)(a => List(a.toString, a.toString))
    }
    println(toString(List(1, 2, 3)) == List("1", "1", "2", "2", "3", "3"))
  }

  def main21(args: Array[String]) {
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B])((a, b) => append(f(a), b))
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if (f(a)) { Nil } else { List(a) })
    }
    def f(n: Int): Boolean = {
      n >= 3
    }
    println(filter(Nil)(f) == Nil)
    println(filter(Cons(4, Nil))(f) == Nil)
    println(filter(Cons(2, Nil))(f) == Cons(2, Nil))
    println(filter(Cons(2, Cons(4, Nil)))(f) == Cons(2, Nil))
  }

  def main22(args: Array[String]) {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
    def zip[A](a1: List[A], a2: List[A]): List[(A, A)] = {
      a1 match {
        case Nil => Nil
        case Cons(h1, t1) => {
          a2 match {
            case Nil          => Nil
            case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))
          }
        }
      }
    }
    def addLists(a1: List[Int], a2: List[Int]): List[Int] = {
      map(zip(a1, a2))(x => x._1 + x._2)
    }
    println(addLists(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  def main23(args: Array[String]) {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
    def zip[A, B](a1: List[A], a2: List[B]): List[(A, B)] = {
      a1 match {
        case Nil => Nil
        case Cons(h1, t1) => {
          a2 match {
            case Nil          => Nil
            case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))
          }
        }
      }
    }
    def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] =
      map(zip(a1, a2))(x => f(x._1, x._2))
    def addLists(a1: List[Int], a2: List[Int]): List[Int] = {
      zipWith(a1, a2)(_ + _)
    }
    println(addLists(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  def main24(args: Array[String]) {
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      sub match {
        case Nil => true
        case Cons(h, t) => {
          sup match {
            case Nil => false
            case Cons(h1, t1) => {
              if (h == h1) {
                if (hasSubsequence(t1, t)) { return true }
              }
              hasSubsequence(t1, sub)
            }
          }
        }
      }
    }
    println(!hasSubsequence(List(1, 2, 3), List(4, 5, 6)))
    println(hasSubsequence(List(1, 2, 3), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3), Nil))
    println(!hasSubsequence(Nil, List(3, 6)))
    println(!hasSubsequence(List(1, 2, 3), List(3, 6)))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(x)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x)             => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def depth(t: Tree[Int]): Int = {
    t match {
      case Leaf(x)             => 1
      case Branch(left, right) => depth(left) + 1 max depth(right) + 1
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x)             => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def main25(args: Array[String]) {
    println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    println(size(Branch(Leaf(1), Leaf(3))) == 3)
  }

  def main26(args: Array[String]) {
    println(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    println(maximum(Branch(Leaf(1), Leaf(3))) == 3)
    println(maximum(Leaf(3)) == 3)
  }

  def main27(args: Array[String]) {
    println(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    println(depth(Branch(Leaf(1), Leaf(3))) == 2)
    println(depth(Leaf(3)) == 1)
  }

  def main28(args: Array[String]) {
    println(
      map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString) == (Branch(
        Leaf("1"),
        Branch(Leaf("2"), Leaf("3")))))
    println(map(Leaf(1))(_.toString) == Leaf("1"))
    println(
      map(Branch(Leaf(2), Leaf(3)))(_.toString) == (Branch(Leaf("2"),
                                                           Leaf("3"))))
  }

  def main29(args: Array[String]) {
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
      t match {
        case Leaf(x)             => f(x)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      }
    }

    def size[A](t: Tree[A]): Int = {
      fold[A, Int](t)(_ => 1)(_ + _ + 1)
    }

    def maximum(t: Tree[Int]): Int = {
      fold[Int, Int](t)(x => x)((x, y) => x max y)
    }

    def depth[A](t: Tree[A]): Int = {
      fold[A, Int](t)(_ => 1)((x, y) => 1 + x.max(y))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
      fold[A, Tree[B]](t)(x => Leaf(f(x)))((left, right) => Branch(left, right))
    }

    println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    println(size(Branch(Leaf(1), Leaf(3))) == 3)

    println(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    println(maximum(Branch(Leaf(1), Leaf(3))) == 3)
    println(maximum(Leaf(3)) == 3)

    println(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    println(depth(Branch(Leaf(1), Leaf(3))) == 2)
    println(depth(Leaf(3)) == 1)

    println(
      map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString) == (Branch(
        Leaf("1"),
        Branch(Leaf("2"), Leaf("3")))))
    println(map(Leaf(1))(_.toString) == Leaf("1"))
    println(
      map(Branch(Leaf(2), Leaf(3)))(_.toString) == (Branch(Leaf("2"),
                                                           Leaf("3"))))

  }
}

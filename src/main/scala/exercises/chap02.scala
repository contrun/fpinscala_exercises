package chap02

object app {

  def main5[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
    f(g(a))
  }

  def main4[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    (f(a))(b)
  }

  def main3[A, B, C](f: (A, B) => C): A => (B => C) = { a: A =>
    { b: B =>
      f((a, b))
    }
  }

  def main2[A](aa: Array[A], greaterThan: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= aa.length - 1) true
      else if (greaterThan(aa(n), aa(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def main1(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, n1: Int, acc: Int): Int =
      if (n <= 1) acc
      else go(n - 1, acc, n1 + acc)
    go(n, 0, 1)
  }

}

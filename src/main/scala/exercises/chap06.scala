package chap06

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n < 0) {
      (math.abs(n + 1), rng2)
    } else {
      (n, rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (math.abs(n.toDouble / Int.MinValue.toDouble), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = double(rng1)
    ((n1, n2), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = double(rng1)
    ((n2, n1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n1, rng1) = double(rng)
    val (n2, rng2) = double(rng1)
    val (n3, rng3) = double(rng2)
    ((n1, n2, n3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // How to do it without resorting to mutability?
    var myRNG: RNG = rng
    var l: List[Int] = List()
    for (i <- 1 to count) {
      val (n, newRNG) = myRNG.nextInt
      myRNG = newRNG
      l = l :+ n
    }
    (l, myRNG)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rnga) = ra(rng)
        val (b, rngb) = rb(rnga)
        (f(a, b), rngb)
      }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    {
      fs match {
        case Nil => (List(), rng)
        case h :: t => {
          val (a, rng1) = h(rng)
          val (ra, rrng) = sequence(t)(rng1)
          (List(a) ++ ra, rrng)
        }
      }
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  val doubleRand: Rand[Double] =
    map[Int, Double](int)(x => math.abs(x.toDouble / Int.MinValue.toDouble))

  def main9(args: Array[String]) {
    def myMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))
    def myMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a =>
        (rng => {
          val (b, rngb) = rb(rng)
          (f(a, b), rngb)
        }))
    }
    val myDoubleInt2: Rand[(Double, Int)] =
      myMap2(doubleRand, int)((x, y) => (x, y))
    val doubleInt2: Rand[(Double, Int)] =
      map2(doubleRand, int)((x, y) => (x, y))
    println(doubleInt2(Simple(1000)))
    println(myDoubleInt2(Simple(1000)))
  }

  def main8(args: Array[String]) {
    def nonNegativeLessThan1(n: Int): Rand[Int] = { rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan1(n)(rng)
    }
    def nonNegativeLessThan2(n: Int): Rand[Int] = {
      flatMap[Int, Int](int)(a => {
        val mod = a % n
        if (a + (n - 1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan2(n)
      })
    }
    println(nonNegativeLessThan1(11)(Simple(2000)))
    println(nonNegativeLessThan2(11)(Simple(2000)))
  }

  def main7(args: Array[String]) {
    println(sequence(List.fill(10)(int))(Simple(1000)))
    println(ints(10)(Simple(1000)))
  }

  def main6(args: Array[String]) {
    val doubleInt2: Rand[(Double, Int)] =
      map2(doubleRand, int)((x, y) => (x, y))
    println(doubleInt2(Simple(1000)))
    println(doubleInt(Simple(1000)))
  }

  def main5(args: Array[String]) {
    val (n1, rng1) = doubleRand(Simple(100))
    val (n2, rng2) = doubleRand(Simple(100))
    println(n1, n2)
  }

  def main4(args: Array[String]) {
    println(ints(10)(Simple(1000)))
  }

  def main3(args: Array[String]) {
    println(intDouble(Simple(1000)))
    println(doubleInt(Simple(1000)))
    println(double3(Simple(1000)))
  }

  def main2(args: Array[String]) {
    val (n1, rng1) = double(Simple(100))
    val (n2, rng2) = double(Simple(100))
    println(n1, n2)
  }

  def main1(args: Array[String]) {
    val (n1, rng1) = nonNegativeInt(Simple(1))
    val (n2, rng2) = nonNegativeInt(Simple(1))
    assert(n1 >= 0)
    assert(n1 == n2)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    this.flatMap(a => sb.flatMap(b => State(s => (f(a, b), s))))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = this.run(s)
      val (b, s2) = f(a).run(s1)
      (b, s2)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def process(input: Input): State[Machine, (Int, Int)] = {
      State(m =>
        input match {
          case Coin => {
            m match {
              case Machine(l, 0, coins) =>
                ((coins, 0), Machine(l, 0, coins))
              case Machine(_, candies, coins) =>
                ((coins + 1, candies), Machine(false, candies, coins + 1))
            }
          }
          case Turn => {
            m match {
              case Machine(l, 0, coins) =>
                ((coins, 0), Machine(l, 0, coins))
              case Machine(true, candies, coins) =>
                ((coins, candies), Machine(true, candies, coins))
              case Machine(false, candies, coins) =>
                ((coins, candies - 1), Machine(true, candies - 1, coins))
            }
          }
      })
    }
    State(m => {
      var fm: Machine = m
      var fs: (Int, Int) = (0, 0)
      for (i <- inputs) {
        val (ts, tm) = process(i).run(fm)
        fs = ts
        fm = tm
      }
      (fs, fm)
    })
  }

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case Nil => State(s => (Nil, s))
      case h :: t => {
        h.map2(sequence(t))((a, b) => a :: b)
      }
    }
  }

  val intRand: Rand[Int] = State(_.nextInt)
  val doubleRand =
    intRand.map[Double](x => math.abs(x.toDouble / Int.MinValue.toDouble))

  def main11(args: Array[String]) {
    val inputs = List(Turn, Coin, Coin, Turn)
    println(simulateMachine(inputs).run(Machine(true, 10, 2)))
  }

  def main10(args: Array[String]) {
    val myDoubleInt: Rand[(Double, Int)] =
      doubleRand.map2(intRand)((x, y) => (x, y))
    println(myDoubleInt.run(Simple(1000)))
    println(sequence(List.fill(10)(intRand)).run(Simple(1000)))
  }
}

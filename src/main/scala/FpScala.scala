# Exercises from the book <Functional programming in Scala>
# TODO - set every exercise number

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x / sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => List()
    case Cons(x,xs) => xs
  }

  // 3.3
  def setHead[A](nHead: A, as: List[A]): List[A] = as match {
    case Cons(x,xs) => Cons(nHead,xs)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => sys.error("Empty list")
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def apply[A] (as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

}

object FpScala {


  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n,1)
  }

  def myfib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else myfib(n-1) + myfib(n-2)
  }

  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n-1, cur, cur + prev)
    loop(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length -1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) =>  f(g(a))

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)


  def main(args: Array[String]): Unit = {
    println(factorial(5))
    println(myfib(3))
    println(fib(5))

    val arr = Array(1,2,3)
    println(findFirst(arr, (x: Int) => x == 2))
    println(isSorted(arr, (x: Int, y: Int) => x < y))

    val f = (x: Double) => math.Pi / 2 - x
    val cos = f andThen(math.sin)

    println(cos)


    val list = List

    println(list.x)

    val intList = List
    val removeFirst = intList.tail(List(1,2,3,4))
    val rFirst = intList.tail(List())

    println(removeFirst)

    val changedHead = intList.setHead(5,List(1,2,3,4))

    val droped = intList.drop(List(1,2,3,4,5),4)

    println(changedHead)
    println(droped)

    val pred = (x: Int) => x <= 5

    val dropWhileList = intList.dropWhile(List(3,4,5,6,7,8), pred)

    println(dropWhileList)

  }
}


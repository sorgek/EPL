package CH3.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](l: List[A], z: B)( f:(A,B) => B): B = l match {
    case Nil => z
    case Cons(x, y) => f(foldRight(y, z)(f))
  }

 def main(args: Array[String]): Unit = {
   println("HelloWorld")
 }
}

List.main("Hi");


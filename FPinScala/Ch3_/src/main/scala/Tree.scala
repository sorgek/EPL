sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => this.size(l) + size(r)
  }

  def max(a:Int, b:Int): Int = if (a > b) a else b

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => max(maximum(l),maximum(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match{
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(a: A => B)(b: (B,B) => B): B = t match{
    case Leaf(x) => a(x)
    case Branch(l,r) => b(fold(l)(a)(b), fold(r)(a)(b))
  }

  def foldMax(t:Tree[Int]): Int = fold(t)(a => a)((a,b) => if(a > b) a else b )
  def foldMap[A, B](t:Tree[A])(f: A=>B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_,_))

  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    a: A => b:B => f(a, b)
  }

  def unCurry[A,B,C](f: A=> B=> C) = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}



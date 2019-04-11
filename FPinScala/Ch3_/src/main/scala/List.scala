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

  def head[A](l: List[A]): A = l match {
    case Cons(x, _) => x
  }

  def append[A](l:List[A], a: A): List[A] = foldRight(l, List(a))(Cons(_,_))

  def product(l: List[Double]) =foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x,y) => Cons(y,x))
  }

  def product2(l: List[Double]) =foldRight(l, 1.0)(_ * _)

  def length[A](l:List[A]): Int =  foldRight(l, 0)((_, y) => y + 1)

  def foldRight2[A,B](l: List[A], z: B)( f:(A,B) => B): B = foldLeft(reverse(l),z)((b,a)=> f(a,b))


  def foldRight[A,B](l: List[A], z: B)( f:(A,B) => B): B = l match {
    case Nil => z
    case Cons(x, y) => f(x, foldRight(y, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](l:List[A], z: B)(f: (B,A) => B): B = l match{
    case Nil => z
    case Cons(x, y) => foldLeft(y, f(z, x))(f)
  }

  def cat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, List[A]())((a, b)=> foldLeft(a, b)((z, x) => append[A](z, x)))
  }

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((x, y) => Cons(x+1,y))

  def toString(l:List[Double]): String = foldRight(l, "")((x,y) => x.toString + "," + y)

  def map[A,B](l: List[A])(f: A=> B): List[B] = {
    foldRight(l, List[B]())((x,y)=>Cons[B](f(x),y))
  }

  def filter[A](l: List[A])(f: A=> Boolean): List[A] = {
    foldRight(l, List[A]())((x,y)=> if (f(x)) Cons[A](x,y) else y)
  }

  def flatMap[A,B](l: List[A])(f: A=> List[B]): List[B] = {
    cat(map(l)(f))
  }

  def flatFilter[A](l: List[A])(f: A=> Boolean): List[A] = {
    flatMap(l)((a)=> if(f(a)) List(a) else Nil)
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case(Nil, _ ) => Nil
    case(_, Nil) => Nil
    case(Cons(t,y), Cons(r, e)) => Cons(f(t,r), zipWith(y, e)(f))

  }

  def startsWith[A](l: List[A], v: A): Boolean = l match{
    case Nil => false
    case Cons(x, _) => x == v
  }

  def hasSub[A](l:List[A], s:List[A]): Boolean = (l,s) match{
    case(_,Nil) => true
    case(Nil, _) => false
    case(Cons(a, b),Cons(x,y)) => if(startsWith(s, a)) hasSub(b, y) else hasSub(b, s)
  }

 def main(args: Array[String]): Unit = {
   val list = List(1,2,3,4,5)
   val list3 = List(1,2,3,4,5)
   val listlist = List(list, list3)
   val list2: List[Double] = List(1,2,3,4,5)
   println("HelloWorld")
   println("Len: " + length(list2))
   println("Reverse: " + reverse(list2))
   println("Append: " + append(list2, 10))
   println("Append: " + cat(listlist))
   println(product(list2))
   println(toString(map(list2)(_+1)))
   println(toString(filter(list2)(_> 2)))
   println("Flatmap:" + toString(flatMap(list)(i => List(i,i))))


   println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
 }
}


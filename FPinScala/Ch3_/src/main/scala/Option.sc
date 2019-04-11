package  whatever

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match { //f iff nn
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  } //apply f which may fail ifnn

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](obj: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse obj
  }

  def filter(f: A => Boolean): Option[A] = {
    if (this.map(f).getOrElse(false) !== None) this else None
  } //converts some to none if not satisfied

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def absO: Option[Double] => Option[Double] = lift(math.abs)

}

object Option {
  def parseInsuranceRateQuote(age: String, tickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(tickets.toInt)
    insuranceRateQuote(age)
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    l.foldRight(List())((a,b)=> List(a.get, b))
  }

  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B) => C): Option[C] = (a,b) {
    a.flatMap( aa => b.map(bb => f(aa,bb)))
    a.map(aa => b.map(bb => f(aa,bb)) getOrElse None)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch{case e: Exception => None }
}





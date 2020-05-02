object Lists {
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case Nil => List()
      case (h:List[_]) :: t => flatten(h) ::: flatten(t)
      case p :: t => List(p) ::: flatten(t)
    }
  }

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x,y) => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)(  (_, acc) => acc + 1 )


  def main(args: Array[String]): Unit = {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println(flatten(List(List(List(List(6))))))
    println(lengthFun(List(1,2,3,4)))
    println(mapFun[Int, Int](List(1,2,3,4), x => (x*x)))
  }
}

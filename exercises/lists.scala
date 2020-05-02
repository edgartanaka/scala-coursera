object lists {
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case Nil => List()
      case h:List[T] :: t => flatten(h) ::: flatten(t)
      case p :: t => List(p) ::: flatten(t)
    }
  }

  def main(args: Array[String]): Unit = {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }
}

object Square {
  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => List()
      case y :: ys => List(y*y) ::: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x*x)

  def main(args: Array[String]): Unit = {
    println(squareList1(List(3,8,4,7,0)))
    println(squareList2(List(3,8,4,7,0)))
  }
}

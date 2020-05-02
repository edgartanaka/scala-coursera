object Pack {

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (packedChars, rest) = xs span (y => x == y)
      packedChars :: pack(rest)
    }
  }

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (packedChars, rest) = xs span (y => x == y)
      (x, packedChars.length) :: encode(rest)
    }
  }


  def main(args: Array[String]): Unit = {
    println(pack(List("a", "a", "a", "b", "c", "c", "a")))
    println(encode(List("a", "a", "a", "b", "c", "c", "a")))
  }

}

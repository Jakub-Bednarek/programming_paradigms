import scala.math.BigInt

val lazyList1: LazyList[Int] =
  def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)
  loop(0)
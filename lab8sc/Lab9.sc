import scala.math.BigInt

val lazyList1: LazyList[Float] =
  def loop(v: Float): LazyList[Float] = v #:: loop(v * 2)
  loop(5)

def transform(llist: LazyList[Float]): LazyList[Float] =
  def to_power_rec(list: LazyList[Float], pos: Float, currentPow: Float, currentVal: Float): LazyList[Float] =
    (currentPow, list) match
      case (_, LazyList()) => LazyList()
      case (x, hd #:: tl) => if pos == currentPow then currentVal #:: to_power_rec(tl, pos + 1, 1, 1)
                             else currentVal #:: to_power_rec(list, pos, currentPow + 1, currentVal * hd)
  to_power_rec(llist, 1, 1, 1)

transform(lazyList1).take(10).toList
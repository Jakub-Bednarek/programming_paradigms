//Jakub Bednarek

//Zadanie 1

def lrepeat[A](k: Int, lxs: LazyList[A]): LazyList[A] = {
  def lrepeatIt(repeats: Int, lList: LazyList[A]): LazyList[A] =
    (repeats, lList) match
      case (_, LazyList()) => LazyList()
      case (0, (_ #:: tail)) => lrepeatIt(k, tail)
      case (_, (head #:: _)) => head #:: lrepeatIt(repeats - 1, lList)
  lrepeatIt(k, lxs)
}

lrepeat(3, LazyList('a', 'b', 'c', 'd')).toList .toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(2, LazyList.from(1).take(5)).toList == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
lrepeat(3, LazyList()) == LazyList()

//Zadanie 2

val lfib : LazyList[Int] = {
    def lfibIt(last: Int, oneBefore: Int) : LazyList[Int] =
      oneBefore #:: lfibIt(last + oneBefore, last)
    lfibIt(1, 1)
}

lfib.take(15).toList == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
lfib.take(10).toList == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
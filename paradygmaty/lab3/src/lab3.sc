import scala.annotation.tailrec

def getPositions[A](a: A, list:List[A]): List[Int] = {
  if (list == Nil) Nil
  else {
    def check(list: List[A], a: A, position: Int, accum: List[Int]): List[Int] = {
      list match {
        case Nil => if (accum == Nil) throw new Exception("Lista nie zawiera podanego elementu!") else accum.reverse
        case _ => if (list.head == a) check(list.tail, a, position + 1, position :: accum)
        else check(list.tail, a, position + 1, accum)
      }
    }
    check(list, a, 0, List())
  }
  }

getPositions(5, List(3, 5, 4, 4, 9, 5, 12, 5))
getPositions(5, List())
getPositions(4, List(3, 5, 7, 2))
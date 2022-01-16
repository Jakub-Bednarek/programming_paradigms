// Zadanie 3
def split(list: List[Int], ele: Int): (List[Int], List[Int]) =
  def splitR(listMain: List[Int], listBelow: List[Int], listAbove: List[Int], ele: Int): (List[Int], List[Int]) =
      if listMain.isEmpty then (listBelow, listAbove)
      else {
          if listMain.head <= ele then splitR(listMain.tail, listBelow ::: List(listMain.head), listAbove, ele)
          else splitR(listMain.tail, listBelow , listAbove ::: List(listMain.head), ele)
         }
  splitR(list, List(), List(), ele)

split(List(1, 2, 3, 4, 5, 6, 7, 8), 5)
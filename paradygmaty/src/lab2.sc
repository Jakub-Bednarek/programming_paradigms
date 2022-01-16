//Zadanie 1
def funcA[A](tuple3:(A, Int, Float)): (List[A], List[Int]) =
  if (tuple3._1 == 3) && (tuple3._2 == 4.5) then (List(tuple3._1, tuple3._1), List(tuple3._2))
  else (List(tuple3._1), List(tuple3._2, tuple3._2))

funcA("pwr", 5, 7.5f)
funcA("uczelnia", 3, 3.0f)

def funcB(tuple2:(List[Float], List[List[Float]])): Boolean =
  if tuple2._1.head == 5.0 && tuple2._2.head.head == 5.0 then true
  else false

funcB(List(3.5f, 4.5f), List(List(1.3f, 2.4f), List(7.5f, 6.4f)))
funcB(List(5.0f), List(List(5.0f, 3.4f), List(), List(2.7f)))

def funcC[A](tuple2:(List[A], List[A])): Int =
  if tuple2._1.head == tuple2._2.head then 1
  else 0

funcC(List(1, 2, 3), List(1, 2, 4))
funcC(List(2, 5, 8), List(3, 6))

//Zadanie 3
def split(list: List[Int], element: Int) : (List[Int], List[Int]) =
  def splitR(listMain: List[Int], listBelow: List[Int], listAbove: List[Int], element: Int): (List[Int], List[Int]) =
    if listMain.isEmpty then (listBelow, listAbove)
    else if listMain.head <= element then splitR(listMain.tail, listBelow ::: List(listMain.head), listAbove, element)
         else splitR(listMain.tail, listBelow, listAbove ::: List(listMain.head), element)

  splitR(list, List(), List(), element)

split(List(), 15)
split(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 6)
split(List(1, 2, 3, 4, 5), 1)
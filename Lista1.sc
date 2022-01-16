// Jakub Bednarek

// Task 1

def flatten1[A](list:List[List[A]]) : List[A] =
  if (list == Nil) Nil
  else list.head ::: flatten1(list.tail)

flatten1(List())
flatten1(List(List(4, 10, 15), List(13, 14, 15)))
flatten1(List(List(4, 10, 15)))
flatten1(List(List(4, 10, 15), List(13, 14, 15), List(155, -354, 3211)))

// Task 2
def count[A](ele:A, list:List[A]) : Int =
  if (list == Nil) 0
  else if (list.head == ele) (1 + count(ele, list.tail))
  else 0 + count(ele, list.tail)

count(1, List())
count(12, List(12, 7, 12, 5, 12, 12, 39))
count(4, List(1, 2, 3, 5, 6, 7, 8))

// Task 3
def replicate[A](ele:A, n:Int) : List[A]=
  if (n == 0) Nil
  else List(ele) ::: replicate(ele, n - 1)

replicate("PWR", 10)
replicate("Uczelnia", 0)
replicate(3, 10)

// Task 4
def sqrList(list:List[Int]) : List[Int] =
  if (list == Nil) Nil
  else List(list.head * list.head) ::: sqrList(list.tail)

sqrList(List())
sqrList(List(1, 3, 5, 12))
sqrList(List(-15, 9, -33))

// Task 5
def palindrome[A](list:List[A]) : Boolean =
  if (list == Nil) true
  else list == list.reverse

palindrome(List())
palindrome(List(1, 3, 5, 3, 1))
palindrome(List("Ala", "Kot", "Tomasz", "Kot", "Ala"))
palindrome(List("Kot", "Tomasz", "Ala"))

// Task 6
def listLength[A](list:List[A]) : Int =
  if (list == Nil) 0
  else 1 + listLength(list.tail)

listLength((List()))
listLength(List(1, 3, 5))
listLength(List("A", "B", "CD", "EFG"))

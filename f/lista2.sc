import scala.annotation.tailrec
//Jakub Bednarek

//Zadanie 2A
def fib(n: Int): Int =
  if (n < 0) throw new Exception ("Wartosc jest nieprawidlowa!")
  else if(n == 0) 0
  else if(n == 1) 1
  else fib(n - 1) + fib(n - 2)

fib(5)
fib(3)
fib(0)
fib(42)
//fib(-1)

//Zadanie 2B
def fibTail(n: Int) =
  def fibIter(n: Int): Int =
    if (n < 0) throw new Exception ("Nieprawidlowa wartosc!")
    else if (n == 0) 0
    else if (n == 1) 1
    else fibIter(n - 1) + fibIter(n - 2)
  fibIter(n)

fibTail(42)
fibTail(9)
fibTail(1)
fibTail(0)
fibTail(5)
fibTail(6)

//Zadanie 3 function
val root3f: Double => Double = (a: Double) => {
  @tailrec
  def root3It(x: Double, a: Double, accum: Double): Double =
    (x, a) match {
      case (x,a) if math.abs(x * x * x - a) <= 1.0E-15 * Math.abs(a) => x
      case _ => root3It(accum, a, x+ (((a / (x * x)) - x)) / 3)
    }
  root3It(if (a <= 1) a else a / 3, a, if (a <= 1) a else a / 3)
}

root3f(27)
root3f(1)
root3f(-27)
root3f(8)
root3f(7)

//Zadanie 3 method
def root3(a: Double): Double = {
  @tailrec
  def root3It(b: Double): Double =
    if (math.abs(b * b * b - a) <= 1.0E-15 * Math.abs(a)) b
    else root3It(b + (a / (b * b) - b) / 3)
  root3It(if (a <= 1) a else a / 3)
}

root3(1)
root3(27)
root3(-27)
root3(8)
root3(7)

//zadanie 4a
val wyr1 = List(-2,-1,0,1,2)
val List(_, _, x, _, _) = wyr1

//zadanie 4b
val wyr2 = List((1,2), (0,1))
val List((_,_), (z,_)) = wyr2

//zadanie 5
def initSegment [A](xs: List[A], ys: List[A]): Boolean =
  (xs, ys) match{
    case (Nil,_) => true
    case (_,Nil) => false
    case _ => if (xs.head == ys.head) initSegment(xs.tail, ys.tail)
    else false
  }

initSegment(List(1,2),List(1,2,3))
initSegment(List(0,1),List(1,2,3))
initSegment(Nil,List(1,2,3))
initSegment(List(1,2),Nil)
initSegment(Nil,Nil)

//zadanie 6
def replaceNth[A](xs: List[A], n:Int, x:A):List[A] =
  (xs, n) match {
    case (Nil,_) => Nil
    case (head :: tail,0) => x :: tail
    case (head :: tail,_) => head :: replaceNth(tail, n - 1, x)
  }

replaceNth(List('o','l','a','m','a','k','o','t','a'),1,'s')
replaceNth(List('1','2','3','4','5','6'),2,'7')
replaceNth(List(1,3,5,7,9),0,0)

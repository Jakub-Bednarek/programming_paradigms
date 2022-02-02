//Jakub Bendarek

//Zadanie 3
class UnderflowException(message: String) extends Exception(message)

class MyQueue[+T] private (private val firstList: List[T], private val lastList: List[T]):
  def first() =
    firstList match
      case hd :: _ => hd
      case Nil => throw new UnderflowException("Queue is empty!")

  def isEmpty() =
    firstList == Nil

  def dequeue() =
    firstList match
      case Nil => new MyQueue[T](Nil, Nil)
      case _ :: Nil =>
        if lastList == Nil then new MyQueue[T](Nil, Nil)
        else new MyQueue[T](lastList.reverse, Nil)
      case _ :: tl => new MyQueue[T](tl, lastList)

  def enqueue[U >: T](x: U) =
    firstList match
      case Nil => new MyQueue[U](List(x), Nil)
      case _ => new MyQueue[U](firstList, x :: lastList)

object MyQueue:
  def apply[T](xs: T*) = new MyQueue[T](xs.toList, Nil)
  def empty[T] = new MyQueue[T](Nil, Nil)

end MyQueue

//Zadanie 4

import scala.collection.mutable.Seq
def copy[T](from: Seq[T], to: Seq[T]) =
  if from.length > to.length then throw new IllegalArgumentException("Object from can't be longer than to!")

  var i = 0
  from.foreach(value =>
    to.update(i, value)
    i = i + 1
  )

object TestList10:
  def main(args: List[String]): Unit =
    //Zadanie 3
    println("Zadanie 3")
    var q1: MyQueue[Any] = MyQueue.empty
    q1 = q1.enqueue(150)
    q1 = q1.enqueue("String")
    q1 = q1.enqueue('c')
    q1 = q1.enqueue(42.15)

    println("First: " + q1.first())
    q1 = q1.dequeue()
    println("First: " + q1.first())
    q1 = q1.dequeue()
    println("First: " + q1.first())
    q1 = q1.dequeue()
    println("First: " + q1.first())
    q1 = q1.dequeue()

    println("\nIs empty: " + q1.isEmpty())

    try
      q1.first()
    catch
      case e: UnderflowException =>
        println(e.getMessage())

    var q2: MyQueue[Float] = MyQueue.empty
    q2 = q2.enqueue(150.0)
    q2 = q2.enqueue(13)
    q2 = q2.enqueue(-5)

    println("\nFirst: " + q2.first())
    q2 = q2.dequeue()
    println("First: " + q2.first())
    q2 = q2.dequeue()
    println("First: " + q2.first())
    q2 = q2.dequeue()

    //Zadanie 4
    println("Zadanie 4")
    var l1: Array[Double] = Array(15.15, 30.1, 89.8, 39.12)
    var l2: Array[Double] = Array(13.4, 15.9, 22.39, 15093.12, 111.222, 333.444, 555.666, 1.1, 2.2, 3.3, 4.4)

    println("\nBefore copy l1: " + l1.toList)
    println("Before copy l2: " + l2.toList)

    copy(l1, l2)

    println("\nAfter copy l1: " + l1.toList)
    println("After copy l2: " + l2.toList)
end TestList10

TestList10.main(List())
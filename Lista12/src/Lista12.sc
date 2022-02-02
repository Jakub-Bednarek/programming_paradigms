import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
//Jakub Bednarek

//Zad 2a
object Zad2a{
  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2

  def main(args: Array[String]): Unit =
    val v1 = pairFut(Future{"Zad2a"}, Future{150})
    val v2 = pairFut(Future{10}, Future{"450"})

    println("Zad2a")
    println(v1)
    println(v2)
}

//Zad 2b
object Zad2b{
  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for
      x <- fut1
      y <- fut2
    yield(x, y)

  def main(args: Array[String]): Unit =
    val v1 = pairFut(Future{"Zad2b"}, Future("134"))
    val v2 = pairFut(Future{188}, Future{"15.43f"})

    println("\nZad2b")
    println(v1)
    println(v2)
}

Zad2a.main(Array[String]())
Zad2b.main(Array[String]())
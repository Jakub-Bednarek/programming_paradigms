import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
//Jakub Bednarek

//Zad 2a
object Zad2a{
  def pairFut2a[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2
}

//Zad 2b
object Zad2b{
  def pairFut2b[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for
      x <- fut1
      y <- fut2
    yield(x, y)

  
}

object Main{
  def main(args: Array[String]): Unit =
    //Zad2a test
    val v1 = pairFut2a(Future{"Zad2a"}, Future{150})
    val v2 = pairFut2a(Future{10}, Future{"450"})

    println("Zad2a")
    println(v1)
    println(v2)

    //Zad2b test
    val v3 = pairFut2b(Future{"Zad2b"}, Future(02022022))
    val v4 = pairFut2b(Future{188}, Future{"15.43f"})

    println("\nZad2b")
    println(v3)
    println(v4)
}

Main.main(Array[String]())
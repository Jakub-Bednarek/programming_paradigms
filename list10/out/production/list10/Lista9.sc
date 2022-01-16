//Jakub Bednarek

//Zadanie 1
class Time_1(Hour: Int):
  private var h = if Hour < 0 then 0 else Hour
  def hour: Int = h
  def hour_=(newHour: Int): Unit =
    if newHour < 0 then h = 0 else h = newHour
  override def toString: String = hour.toString
end Time_1

//Zadanie 2a
class Time_2a(private var Hour: Int, private var Minutes: Int):
  require(Hour >= 0 && Hour < 24, s"newHour out of bounds![0, 23]")
  require(Minutes >= 0 && Minutes < 60, s"newMinutes out of bounds![0, 59]")

  def hour: Int = Hour
  def hour_=(newHour: Int): Unit =
    require(newHour >= 0 && newHour < 24, s"newHour out of bounds![0, 23]")
    Hour = newHour

  def minutes: Int = Minutes
  def minutes_=(newMinutes: Int): Unit =
    require(newMinutes >= 0 && newMinutes < 60, s"newMinutes out of bounds![0, 59]")

  def before(other: Time_2a): Boolean =
    if Hour == other.Hour then Minutes < other.Minutes
    else Hour < other.Hour

  override def toString: String = "(" + Hour.toString + ", " + Minutes.toString + ")"
end Time_2a

//Zadanie 2b
class Time_2b(private var Minutes: Int):
  require(Minutes >= 0 && Minutes < 1440, s"Time out of bounds![0, 1439]")

  def hour: Int = Minutes / 60
  def hour_=(newHour: Int): Unit =
    require(newHour >= 0 && newHour < 24, s"newHour out of bounds![0, 23]")
    Minutes = Minutes - (hour * 60) + (newHour * 60)

  def minutes: Int = Minutes - (hour * 60)
  def minutes_=(newMinutes: Int): Unit =
    require(newMinutes >= 0 && newMinutes < 60, s"newMinutes out of bounds![0, 59]")
    Minutes = Minutes - minutes + newMinutes

  def before(other: Time_2b): Boolean =
    return Minutes < other.Minutes

  override def toString: String = "(" + hour.toString + ", " + minutes.toString + ")"
end Time_2b

//Zadanie 3
val default_prod_year = -1
val default_reg_number = ""

class Pojazd(private val producent: String, private val model: String,
             private val prod_year: Int, private var reg_number: String):
  def this(producent: String, model: String) =
    this(producent, model, default_prod_year, default_reg_number)

  def this(producent: String, model: String, prod_year: Int) =
    this(producent, model, prod_year, default_reg_number)

  def this(producent: String, model: String, reg_number: String) =
    this(producent, model, default_prod_year, reg_number)

  override def toString: String = "(" + producent + ", " + model + ", " + prod_year.toString + ", " + reg_number + ")"
end Pojazd

//Zadanie 4
def metoda3(): Unit = throw new Exception("Wyjatek zgloszony w metoda 3")
def metoda2(): Unit = metoda3()
def metoda1(): Unit = metoda2()

object TestList9:
  def main(args: Array[String]): Unit =
    //Zadanie 1
    println("Zadanie 1")
    val t1 : Time_1 = new Time_1(15)
    val t2 : Time_1 = new Time_1(-50)
    val t3 : Time_1 = new Time_1(0)

    println("t1: " + t1)
    println("t2: " + t2)
    println("t3: " + t3)
    println()

    println("t1 = " + t1)
    t1.hour = -1000
    println("t1 = " + t1)
    t1.hour = 30
    println("t1 = " + t1)

    //Zadanie 2a
    println("\nZadanie2a")
    val t1_a = new Time_2a(15, 30)
    val t2_a = new Time_2a(3, 45)

    println("t1_a = " + t1_a)
    println("t2_a = " + t2_a)

    println("t1_a before t2_a: " + t1_a.before(t2_a))
    println("t2_a before t1_a: " + t2_a.before(t1_a))

    t1_a.hour = 3
    println("t1_a = " + t1_a)
    println("t1_a before t2_a: " + t1_a.before(t2_a))

    t1_a.minutes = 50
    println("t1_a = " + t1_a)
    println("t1_a before t2_a: " + t1_a.before(t2_a))

    //Zadanie 2b
    println("\nZadanie2b")
    val t1_b = new Time_2b(129)
    val t2_b = new Time_2b(398)
    val t3_b = new Time_2b(1200)
    //t3_b.minutes = -3

    println("t1_b = " + t1_b)
    println("t2_b = " + t2_b)
    println("t3_b = " + t3_b)

    println("t1_b before t2_b: " + t1_b.before(t2_b))
    println("t2_b before t1_b: " + t2_b.before(t1_b))

    t1_b.minutes = 50
    t2_b.minutes = 10

    println("t1_b = " + t1_b)
    println("t2_b = " + t2_b)

    t1_b.hour = 10
    t2_b.hour = 3

    println("t1_b = " + t1_b)
    println("t2_b = " + t2_b)

    //Zadanie 3
    println("\nZadanie 3")
    var p1: Pojazd = new Pojazd("Opel", "Astra")
    var p2: Pojazd = new Pojazd("Audi", "A4", 2001)
    var p3: Pojazd = new Pojazd("Ford", "Focus", "DW338412")
    var p4: Pojazd = new Pojazd("Ferrari", "458 Italia", 2019, "F458IT")

    println("p1: " + p1)
    println("p2: " + p2)
    println("p3: " + p3)
    println("p4: " + p4)

    //Zadanie 4
    try
      metoda1()
    catch
      case e: Exception =>
        println(e.getMessage())
        e.printStackTrace()

TestList9.main(Array())
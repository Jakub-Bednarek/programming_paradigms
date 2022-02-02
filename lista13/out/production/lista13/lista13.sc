class Buffer:
  private var msg: String = ""
  private var empty = true

  def put(msg: String) = this.synchronized {
    while !empty do wait()
    if msg != "Done"
      then println(s"Do Gabinetu wchodzi $msg")
      else println(s"Do Gabinetu nie wchodzi juz nikt, Doktor ma wolne")
    this.msg = msg
    empty = false
    notifyAll()
  }

  def take: String = this.synchronized {
    while empty do wait()
    empty = true
    notifyAll()
    if msg != "Done"
      then println(s"Z Gabinetu wychodzi $msg")
      else println(s"Z Gabinetu nie wychodzi juz nikt, Doktor ma wolne")
    msg
  }
end Buffer

class Lekarz(buf: Buffer) extends Thread("LekarzThread"):
  override def run: Unit =
    var msg = ""
    while
      msg = buf.take
      msg != "Done"
    do {}
end Lekarz

class Pacjent(max_pacjencji: Int, buf: Buffer) extends Thread("PacjentThread"):
  override def run: Unit =
    for i <- 1 to max_pacjencji do buf.put(s"Pacjent $i")
    buf.put("Done")
end Pacjent

class Buffer2:
  private var current_approx: Double = 0.0
  private var finished = false
  def put(v: Double) = this.synchronized {
    while current_approx != 0.0 do wait()
    this.current_approx = v
    notifyAll()
  }

  def take: Double = this.synchronized {
    while current_approx == 0.0 do wait()
    var v = current_approx
    current_approx = 0.0
    notifyAll()
    v
  }

  def put_finished: Unit = this.synchronized {
    finished = true
  }

  def take_finished: Boolean = this.synchronized {
    finished
  }
end Buffer2

class Iloczyn(buf: Buffer2) extends Thread("IloczynThread"):
  override def run: Unit =
    var i = 1
    while !(buf.take_finished) do
      var ilocz = (2 * i) / ((2 * i) - 1) * (2 * i) / ((2 * i) + 1)
      buf.put(ilocz)
      println(s"Iloczyn: $ilocz")
      i = i + 1
end Iloczyn

class Przybl(przybl: Double, buf: Buffer2) extends Thread("PrzyblThread"):
  override def run: Unit =
    var last_approx = 0.0
    var current_approx = 0.0
    var diff = 10000000.0
    while (diff > przybl) do {
      var v = buf.take
      last_approx = current_approx
      current_approx = current_approx * v
      diff = Math.abs(current_approx - last_approx)
      println(s"Diff: $diff")
    }
    buf.put_finished
    println(s"Wyznaczona liczba PI: $current_approx")


object Main:
  def main(args: Array[String]): Unit =
    println("Start")
    var buf = new Buffer
    new Pacjent(10, buf).start()
    new Lekarz(buf).start()
end Main

object Main2:
  def run: Unit =
    println("Start2")
    var buf2 = new Buffer2
    new Iloczyn(buf2).start()
    //new Przybl(0.0001, buf2).start()
end Main2

Main.main(Array[String]())
Main2.run
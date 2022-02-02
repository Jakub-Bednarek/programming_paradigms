class Zwierzak(val gatunek: String, val imie: String, val data_urodzenia: String):
  override def toString: String = "(" + gatunek + ", " + imie + ", " + data_urodzenia + ")"
end Zwierzak

object Obora:
  private var next_nr = 0
  def get_next_nr: Int = next_nr
  def increment: Unit = next_nr = next_nr + 1

class Obora(private val wlasciciel: String, private val liczba_boxow: Int):
  private var boxy: List[Zwierzak] = List[Zwierzak]()
  private var nr: Int = Obora.get_next_nr
  Obora.increment

  def get_nr: Int = nr

  def dodaj_zwierzaka(zwierzak: Zwierzak) =
    if boxy.length < liczba_boxow then boxy = boxy :+ zwierzak
    else println("Wszystkie boxy sa pelne!")

  def usun_zwierzaka(imie: String) =
    var nowe_boxy: List[Zwierzak] = List[Zwierzak]()

    for(i <- 0 until boxy.length)
      if(boxy(i).imie != imie) then nowe_boxy = nowe_boxy :+ boxy(i)

    boxy = nowe_boxy

  def wyswietl_liste() =
    for(i <- boxy)
      println(i)

  private def czy_pelna(): Boolean =
    return boxy.length == liczba_boxow

  def przenies_zwierzaka(imie: String, nowa_obora: Obora) =
    if(!nowa_obora.czy_pelna()) {
      var stara_dlugosc = boxy.length
      usun_zwierzaka(imie)

      if stara_dlugosc != boxy.length
      then nowa_obora.dodaj_zwierzaka(imie)
      else println("Nie ma takiego zwierzaka w oborze!")
    }

end Obora

var ob1: Obora = new Obora( "Bednarek", 5)
var zw1: Zwierzak = new Zwierzak("Labrador", "Puszek", "19.02.2019")
var zw2: Zwierzak = new Zwierzak("Dalmatynczyk", "Okruszek", "10.10.2003")
var zw3: Zwierzak = new Zwierzak("Pingwin", "Mleczyk", "17.15.2005")
var zw4: Zwierzak = new Zwierzak("Golab", "Roza", "5.15.2002")
var zw5: Zwierzak = new Zwierzak("Strus", "Wilkus", "8.15.2007")
var zw6: Zwierzak = new Zwierzak("Szympans", "Krzykacz", "12.15.2006")
ob1.dodaj_zwierzaka(zw1)
ob1.dodaj_zwierzaka(zw2)
ob1.dodaj_zwierzaka(zw3)
ob1.dodaj_zwierzaka(zw4)
ob1.dodaj_zwierzaka(zw5)
ob1.dodaj_zwierzaka(zw6)

ob1.wyswietl_liste()

ob1.usun_zwierzaka("Dalmatynczyk")

ob1.wyswietl_liste()

var ob2: Obora = new Obora( "Krasicki", 3)
var zw7: Zwierzak = new Zwierzak("Pekinczyk", "Bialek", "03.06.2007")
var zw8: Zwierzak = new Zwierzak("York", "Malek", "24.05.2013")
var zw9: Zwierzak = new Zwierzak("Pletwal Blekitny", "Olbrzym", "13.05.1980")

ob2.dodaj_zwierzaka(zw7)
ob2.dodaj_zwierzaka(zw8)

println("Zawartosc obory nr: " + ob1.get_nr)
ob1.wyswietl_liste()

println("Zawartosc obory nr: " + ob2.get_nr)
ob2.wyswietl_liste()

ob1.przenies_zwierzaka("Pingwin", ob2)

println("Po przeniesieniu:")
println("Zawartosc obory nr: " + ob1.get_nr)
ob1.wyswietl_liste()

println("Zawartosc obory nr: " + ob2.get_nr)
ob2.wyswietl_liste()

var ob3: Obora = new Obora("Mickiewicz", 6)
var ob4: Obora = new Obora("Malysz", 6)
var ob5: Obora = new Obora("Kubica", 6)

println("Numer obory ob3: " + ob3.get_nr)
println("Numer obory ob3: " + ob4.get_nr)
println("Numer obory ob3: " + ob5.get_nr)
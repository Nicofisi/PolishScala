package me.nicofisi.polishscala

object PolishScala extends App {
  type TakLubNie = Boolean
  type LiczbaCałkowita = Int
  type Bajt = Byte
  type KrótkaLiczbaCałkowita = Short
  type DługaLiczbaCałkowita
  type Cokolwiek = Any
  type Warunek = () => TakLubNie
  type Nic = Nothing
  type NicNieZwraca = Unit
  type Lista[+A] = List[A]

  val PustaLista: Nil.type = Nil

  def wydrukujLinię(cokolwiek: Cokolwiek): Unit = println(cokolwiek)

  case class WyrażenieWarunkowe[A](warunek: Warunek, kod: () => A) {
    var subwyrażenia: List[WyrażenieWarunkowe[A]] = Nil
    var wPrzeciwnymWypadku: Option[() => A] = None

    def lubJeżeli(warunek: Warunek)(kod: () => A): this.type = {
      subwyrażenia = WyrażenieWarunkowe(warunek, kod) :: subwyrażenia
      this
    }

    def wPrzeciwnymWypadku(kod: () => A): this.type = {
      wPrzeciwnymWypadku = Some(kod)
      this
    }

    def sprawdź(): Unit = if (warunek()) kod() else {
      subwyrażenia.reverse.find(_.warunek()) match {
        case Some(subwyrażenie) => subwyrażenie.kod()
        case None => wPrzeciwnymWypadku.map(_ ())
      }
    }

    def sprawdźZRezultatem(): A = if (warunek()) kod() else {
      subwyrażenia.reverse.find(_.warunek()) match {
        case Some(subwyrażenie) => subwyrażenie.kod()
        case None => wPrzeciwnymWypadku.get()
      }
    }
  }

  def róbDopóki[A](warunek: Warunek)(kod: () => A): Unit = while (warunek()) kod()

  def jeżeli[A](warunek: Warunek)(kod: () => A): WyrażenieWarunkowe[A] = WyrażenieWarunkowe(warunek, kod)


  // Some examples

  jeżeli {() => 2 > 3} { () =>
    wydrukujLinię("tak")
  }.lubJeżeli {() => 10 == 10} { () =>
    wydrukujLinię("drugie tak")
  }.wPrzeciwnymWypadku { () =>
    wydrukujLinię("nie")
  } sprawdź()


  val warunek: TakLubNie = true

  wydrukujLinię(jeżeli {() => warunek} {() => "tak"}.wPrzeciwnymWypadku {() => "nie"}.sprawdźZRezultatem())


  var i = 0

  róbDopóki {() => i < 10} { () =>
    i += 1
    wydrukujLinię(s"i to $i")
  }
  wydrukujLinię("gotowe")
}

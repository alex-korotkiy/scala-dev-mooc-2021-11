package collections

import scala.collection.immutable.HashSet

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    text.zipWithIndex.collect({
      case (s, i) =>
        if (i==0) s
        else
          if (isASCIIString(s)) s.toUpperCase
          else s.toLowerCase
    })

  }

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  val numbers1_19 = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen")
  val numbers20_90 = Map(20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty", 60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety")
  val numbers_tmb = Map(3 -> "thousand", 6-> "million", 9-> "billion")

  def nonEmptyJoin(sep: String, strings: String*) = strings.filter(s=> s!="").mkString(sep)

  def positiveInt2Words(n: Int): String = {
    n match {
      case 0 => ""
      case n if n>=1 && n<20 => numbers1_19(n)
      case n if n>=20 && n<100 => nonEmptyJoin(" ", numbers20_90((n/10)*10), positiveInt2Words(n%10))
      case n if n>=100 && n<1000 => nonEmptyJoin(" ", positiveInt2Words(n/100), "hundred", positiveInt2Words(n%100))
      case _ => {
        val zeros_count = ((n.toString.length - 1)/3)*3
        val d = math.pow(10, zeros_count).toInt
        nonEmptyJoin(" ", positiveInt2Words(n/d), numbers_tmb(zeros_count), positiveInt2Words(n%d))
      }
    }
  }

  def numString2Words(numString: String): String = {
    numString.toIntOption match {
      case Some(n) => if (n==0) "zero" else positiveInt2Words(n)
      case None => numString
    }
  }

  def numbersToNumericString(text: String): String = {
    val collected = new StringBuilder
    var numberFound = ""
    for (i <- 0 until(text.length)){
      val sym = text(i).toString
      sym.toIntOption match {
        case Some(_) => numberFound = numberFound + sym
        case None => {
          collected.append(numString2Words(numberFound))
          collected.append(sym)
          numberFound = ""
        }
      }
    }
    collected.append(numString2Words(numberFound))
    collected.toString
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    dealerOne.toSet union dealerTwo.toSet
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    dealerOne.toSet diff dealerTwo.toSet
  }
}

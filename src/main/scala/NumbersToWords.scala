/**
  * Will turn a number between {{{0}}} and {{{999,999,999}}} to it's English representation.
  *
  * Here is a good online example: [[https://lingojam.com/NumbersToWords]]
  */
object NumbersToWords {

  private val UnderZero: PartialFunction[Int, String] = {
    case x if x < 0 => throw new IllegalArgumentException
  }

  private val Zero: PartialFunction[Int, String] = {
    case 0 => "zero"
  }

  private val Singles: PartialFunction[Int, String] = {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
  }

  private val Teens: PartialFunction[Int, String] = {
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
  }

  private val Tens: PartialFunction[Int, String] = {
    case 10 => "ten"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
  }

  private val TensAndSingles = new PartialFunction[Int, String] {
    def isDefinedAt(num: Int): Boolean = num == 10 || (num >= 20 && num <= 99)

    def apply(num: Int): String = {
      val remainder = num % 10
      Tens(num - remainder) + (if (remainder == 0) "" else "-" + Singles(remainder))
    }
  }

  private val UnderHundred = TensAndSingles orElse Teens orElse Singles

  private def by00(by: Int, first000: Boolean) = new PartialFunction[Int, String] {
    def isDefinedAt(num: Int): Boolean = num >= by

    def apply(num: Int): String =
      UnderHundred(num / by) + " hundred" + (num % by match {
        case 0 => ""
        case x if Singles.isDefinedAt(x) => (if (first000) " " else " and ") + Singles(x)
        case 10 => (if (first000) " " else " and ") + Tens(10)
        case x if Teens.isDefinedAt(x) => (if (first000) " " else " and ") + Teens(x)
        case x if Tens.isDefinedAt(x) => " " + Tens(x)
        case x => " and " + TensAndSingles(x)
      })
  }

  private def by000(by: Int, byStr: String, under: PartialFunction[Int, String]) = new PartialFunction[Int, String] {
    def isDefinedAt(num: Int): Boolean = num >= by

    def apply(num: Int): String =
      (UnderHundred orElse by00(100, first000 = true)) (num / by) + s" $byStr" + (num % by match {
        case 0 => ""
        case x => " " + under(x)
      })
  }

  private val Hundreds = by00(100, first000 = false)

  private val UnderThousand = Hundreds orElse UnderHundred

  private val Thousands = by000(1000, "thousand", UnderThousand)

  private val UnderMillion = Thousands orElse UnderThousand

  private val Millions = by000(1000000, "million", UnderMillion)

  private val BillionAndOver: PartialFunction[Int, String] = {
    case x if x >= 1000000000 => throw new IllegalArgumentException
  }

  def apply(num: Int): String =
    (BillionAndOver orElse Millions orElse UnderMillion orElse Zero orElse UnderZero) (num)
}
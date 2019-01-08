import org.scalatest._

class NumbersToWordsSpec extends FlatSpec with Matchers {

  "NumbersToWords" should "work for singles" in {
    NumbersToWords(0) should be("zero")
    NumbersToWords(1) should be("one")
    NumbersToWords(2) should be("two")
    NumbersToWords(3) should be("three")
    NumbersToWords(4) should be("four")
    NumbersToWords(5) should be("five")
    NumbersToWords(6) should be("six")
    NumbersToWords(7) should be("seven")
    NumbersToWords(8) should be("eight")
    NumbersToWords(9) should be("nine")
  }

  it should "work with the teens" in {
    NumbersToWords(11) should be("eleven")
    NumbersToWords(12) should be("twelve")
    NumbersToWords(13) should be("thirteen")
    NumbersToWords(14) should be("fourteen")
    NumbersToWords(15) should be("fifteen")
    NumbersToWords(16) should be("sixteen")
    NumbersToWords(17) should be("seventeen")
    NumbersToWords(18) should be("eighteen")
    NumbersToWords(19) should be("nineteen")
  }

  it should "work for the tens" in {
    NumbersToWords(10) should be("ten")
    NumbersToWords(20) should be("twenty")
    NumbersToWords(30) should be("thirty")
    NumbersToWords(40) should be("forty")
    NumbersToWords(50) should be("fifty")
    NumbersToWords(60) should be("sixty")
    NumbersToWords(70) should be("seventy")
    NumbersToWords(80) should be("eighty")
    NumbersToWords(90) should be("ninety")
  }

  it should "work for the tens and singles" in {
    NumbersToWords(21) should be("twenty-one")
    NumbersToWords(32) should be("thirty-two")
    NumbersToWords(43) should be("forty-three")
    NumbersToWords(54) should be("fifty-four")
    NumbersToWords(65) should be("sixty-five")
    NumbersToWords(76) should be("seventy-six")
    NumbersToWords(87) should be("eighty-seven")
    NumbersToWords(98) should be("ninety-eight")
  }

  it should "work for larger amounts with missing denominations" in {
    NumbersToWords(100) should be("one hundred")
    NumbersToWords(15000) should be("fifteen thousand")
    NumbersToWords(10000000) should be("ten million")
    NumbersToWords(44000000) should be("forty-four million")
    NumbersToWords(4500000) should be("four million five hundred thousand")
    NumbersToWords(17000200) should be("seventeen million two hundred")
    NumbersToWords(60010000) should be("sixty million ten thousand")
    NumbersToWords(600010000) should be("six hundred million ten thousand")
  }

  it should "work for some unusual cases" in {
    NumbersToWords(104) should be("one hundred and four")
    NumbersToWords(120) should be("one hundred twenty")
    NumbersToWords(181) should be("one hundred and eighty-one")
    NumbersToWords(1040) should be("one thousand forty")
    NumbersToWords(10403) should be("ten thousand four hundred and three")
    NumbersToWords(104030) should be("one hundred four thousand thirty")
    NumbersToWords(1040305) should be("one million forty thousand three hundred and five")
    NumbersToWords(10403050) should be("ten million four hundred three thousand fifty")
    NumbersToWords(104030506) should be("one hundred four million thirty thousand five hundred and six")
    NumbersToWords(121121) should be("one hundred and twenty-one thousand one hundred and twenty-one")
    NumbersToWords(999999999) should be("nine hundred and ninety-nine million nine hundred and ninety-nine thousand nine hundred and ninety-nine")
  }

  it should "not support minus numbers or over 999,999,999" in {
    an[IllegalArgumentException] should be thrownBy NumbersToWords(-1)
    an[IllegalArgumentException] should be thrownBy NumbersToWords(1000000000)
  }
}

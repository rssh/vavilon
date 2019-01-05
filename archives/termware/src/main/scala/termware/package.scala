import termware.plain._

import scala.language.implicitConversions


package object termware  {

  implicit def byteToTerm(b:Byte): ByteTerm = ByteTerm(b)

  implicit def charToTerm(ch:Char): CharTerm = CharTerm(ch)

  implicit def intToTerm(v:Int): IntTerm = IntTerm(v)

  implicit def bigDecimalToTerm(v:BigDecimal): BigDecimalTerm = BigDecimalTerm(v)

  implicit def stringToTerm(s:String): StringTerm = StringTerm(s)




}

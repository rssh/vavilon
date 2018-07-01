package termware

import fastparse.all._

object TermwareParser {

  val snumber:P[String] = P( CharIn('0' to '9')).rep(1).!

  val longNumber = snumber.map(_.toLong)

  val floatingPoint:P[Double] = (snumber ~ P(".") ~ snumber).!.map(_.toDouble)

  val escapeChar = P("\\") ~ (P("\"").map(_ => '\\')|
                               P("n").map(_ => '\n')
    )

  val insideString:P[String] = (CharsWhile(ch => ch != '"' && ch != '\\')|escapeChar).rep(0).!


  val string = P("\"") ~/ insideString ~/ P("\"")


  val boolean = P("true").map(_ => true)|P("false").map(_ => false)

  val identifier: P[String] = CharPred(_.isLetter) ~ CharsWhile(_.isLetterOrDigit).!

  // terms.

  val longTerm:P[PointTerm] = longNumber.map(LongTerm)

  val floatingPointTerm:P[PointTerm] = floatingPoint.map(DoubleTerm)

  val booleanTerm = boolean.map(BooleanTerm)

  val stringTerm = string.map(StringTerm)

  val atomTerm = identifier.map(AtomTerm(_))


  val primitiveTerm: P[PointTerm] = longTerm | floatingPointTerm| booleanTerm | stringTerm


}

package termware


import scala.language.implicitConversions



trait DSL {

  def atom(s:String) = AtomTerm(s)

  implicit def symbolToAtom(symbol:Symbol): AtomTerm = AtomName(symbol.name)

  @inline final def star = StarTerm.U
  @inline final def STAR = StarTerm.U

  type BooleanTerm = PrimitiveTerm[Boolean]
  implicit def booleanToBooleanTerm(value: Boolean): BooleanTerm = BooleanTerm(value)

  type IntTerm = PrimitiveTerm[Int]
  implicit def intToIntTerm(value: Int): IntTerm = IntTerm(value)

  type LongTerm = PrimitiveTerm[Long]
  implicit def longToLongTerm(value: Long): LongTerm = LongTerm(value)

  type StringTerm = PrimitiveTerm[String]
  implicit def stringToStringTerm(value: String): StringTerm = StringTerm(value)


  implicit def pairTranslate[A,B](x:(A,B))(implicit ac:A=>AtomTerm,bd:B=>MultiTerm):(AtomTerm,MultiTerm) =
    (ac(x._1),bd(x._2))


}

object DSL extends DSL

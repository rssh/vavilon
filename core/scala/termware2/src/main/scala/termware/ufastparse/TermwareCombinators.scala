package termware.ufastparse

import fastparse._
import fastparse.NoWhitespace._
import termware._
import termware.util.NameIndexed

object TermwareCombinators {


  def snumber[_:P] : P[String] = P( CharsWhileIn("0-9").! )

  def longNumber[_:P]: P[Long] = P( snumber.map(_.toLong) )


  def floatingPoint[_:P]: P[Double] = P(CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9")).!.map( _.toDouble )


  def escapeChar[_ : P] : P[String] = P("\\" ~ ("\"" | "\\") ).!

  def insideString[_: P] :P[String] = P((CharsWhile(ch => ch != '"' && ch != '\\')|escapeChar).rep(1).!)


  def string[_ :P]: P[String] = P("\"") ~/ insideString ~/ P("\"")


  def boolean[_ :P]: P[Boolean] = P("true").map(_ => true)|P("false").map(_ => false)

  def identifier[_: P]: P[String] = P(CharsWhile(_.isLetter) ~ CharsWhile(_.isLetterOrDigit)).!

  // terms.

  def longTerm[_: P]:P[PointTerm] = longNumber.map(LongTerm)

  def floatingPointTerm[_:P]:P[PointTerm] = floatingPoint.map(DoubleTerm)

  def booleanTerm[_:P]:P[PrimitiveTerm[Boolean]] = boolean.map(BooleanTerm)

  def stringTerm[_:P]:P[PrimitiveTerm[String]] = string.map(StringTerm)

  def atomTerm[_:P] = identifier.map(x => AtomTerm(x))

  def primitiveTerm[_:P]: P[PointTerm] =  P( floatingPointTerm | longTerm | booleanTerm | stringTerm )

  def structuredTerm[_:P]: P[StructuredTerm] =
    (atomTerm ~ "(" ~ termArgsTail ).map{ case (x,tail) =>
      val s0 = (NameIndexed[MultiTerm](),1)
      val (subterms, n) = tail.foldLeft(s0){ (s,e) =>
        val (indexes,count) = s
        val (name,value) = e
        val nName = if (name == KernelNames.emptyName) {
                       AtomTerm("p"+count)
                    } else {
          name
        }
        (indexes.updated(nName.name,value),count+1)
      }
      StructuredTerm.create(x,subterms)
    }


  def termArgsTail[_:P]: P[List[(AtomTerm,MultiTerm)]] = (
    P(")").map(_ => Nil)
    |
    P("," ~ termArg ~ termArgsTail).map{case (x,y,z) =>
      (x,y)::z
      }
    )

  def termArg[_:P]: P[(AtomTerm,MultiTerm)] = (
     atomTerm ~ termArgAtomTail
    |
     multiTerm.map(t => (KernelNames.emptyName, t))
    )


  def termArgAtomTail[_:P] :P[MultiTerm] = (
    P("=" ~ multiTerm)
    |
    &(",").map(_ => EmptyTerm)
  )

  def pointTerm[_:P]: P[PointTerm] = atomTerm | primitiveTerm | structuredTerm

  def multiTerm[_:P]: P[MultiTerm] = pointTerm

  def expression[_:P] = Start ~ multiTerm



}

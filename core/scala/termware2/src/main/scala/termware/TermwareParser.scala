package termware

import fastparse.all._
import termware.util.FastParseLocAttributed._
import termware.util.NameIndexed

object TermwareParser {


  val snumber:P[InSource[String]] = laCharsWhile(_.isDigit)

  val longNumber:P[InSource[Long]] = snumber.map(_.map(_.toLong))


  val floatingPoint:P[InSource[Double]] = (snumber ~ laLiteral(".") ~ snumber).map{
    case (n1,c,n2) => combine3[String,String,String,Double]( (x,y,z) => (x + y + z).toDouble )(n1,c,n2)
  }

  val escapeChar:P[InSource[String]] = (laLiteral("\\") ~ (
                               laLiteral("\"").map(_.map( _ => '"'))
                                 |
                               laLiteral("n").map(_.map(_ => '\n'))
    )).map(_._2.map(_.toString))

  val insideString:P[InSource[String]] = (laCharsWhile(ch => ch != '"' && ch != '\\')|escapeChar).rep(1).map{
    combineSeq(_.mkString(""))
  }


  val string = P("\"") ~/ insideString ~/ P("\"")


  val boolean = laLiteral("true").map(_.map(_ => true))|laLiteral("false").map(_.map(_ => false))

  val identifier: P[InSource[String]] = (laCharPred(_.isLetter) ~ laCharsWhile(_.isLetterOrDigit)).map{
    case (x,y) => combine2[Char,String,String](_.toString + _ )(x,y)
  }

  // terms.

  val longTerm:P[InSource[PointTerm]] = longNumber.map(_.map(LongTerm))

  val floatingPointTerm:P[InSource[PointTerm]] = floatingPoint.map(_.map(DoubleTerm))

  val booleanTerm = boolean.map(_.map(BooleanTerm))

  val stringTerm = string.map(_.map(StringTerm))

  val atomTerm = identifier.map(_.map(AtomTerm(_)))

  val primitiveTerm: P[InSource[PointTerm]] = longTerm | floatingPointTerm| booleanTerm | stringTerm

  lazy val structuredTerm: P[InSource[StructuredTerm]] =
    (atomTerm ~~ laLiteral("(") ~~ termArgsTail ).pmap{ case ((x,y),z) =>
      val s0 = (NameIndexed[MultiTerm](),1)
      val (subterms, n) = z.foldLeft(s0){ (s,e) =>
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


  lazy val termArgsTail: P[InSource[List[(AtomTerm,MultiTerm)]]] =(
    laLiteral(")").pmap(_ => Nil)
    |
    (laLiteral(",") ~~ termArg ~~ termArgsTail ).pmap{case ((x,y),z) =>
         y::z
      }
    )

  lazy val termArg: P[InSource[(AtomName,MultiTerm)]] = (
     atomTerm ~~ termArgAtomTail
    |
     multiTerm.pmap(t => (KernelNames.emptyName, t))
    )


  lazy val termArgAtomTail:P[InSource[MultiTerm]] =
    ((laLiteral("=") ~~ multiTerm).pmap{case (x,y) => y}
    |
    (&(laLiteral(","))).map(_ => InSource(EmptyTerm,InSourceLocation.ZERO))
   )

  lazy val pointTerm: P[InSource[PointTerm]] = atomTerm | primitiveTerm | structuredTerm

  lazy val multiTerm: P[InSource[MultiTerm]] = pointTerm

}

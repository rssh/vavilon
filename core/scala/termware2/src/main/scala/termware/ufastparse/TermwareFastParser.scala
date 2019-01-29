package termware.ufastparse

import fastparse.Parsed
import termware._
import termware.util.{ParseError, SourceLocation, TermwareParser}

class TermwareFastParser(in: IndexedParserInput) extends TermwareParser {

  override type Position = Int

  override def sourceName: String = in.fname()

  override def readTerm(): Either[ParseError,MultiTerm] = {
    parsedToEither(fastparse.parse(in, TermwareCombinators.multiTerm(_), true))
  }

  override def readPrimitive[T](ops: PrimitiveTermOps[T] ): Either[ParseError,PrimitiveTerm[T]] = {
    for{ t <- parsedToEither(fastparse.parse(in, TermwareCombinators.primitiveTerm(_), true ))
         pt <- t.kind match {
           case k: PrimitiveTermKind => val p = k.primitive(k.pointTerm(t))
             if (p.primitiveTypeIndex == ops.primitiveTypeIndex) {
               Right(p.asInstanceOf[PrimitiveTerm[T]])
             } else {
               Left(ParseError(s"primitive typeIndex should be ${ops.primitiveTypeIndex}, we have ${p.primitiveTypeIndex} ",SourceLocation(sourceName,0,0)))
             }
           case other =>
             Left(ParseError(s"not a primitive term",SourceLocation(sourceName,0,0)))
         }
    } yield pt
  }

  override def readAtom(): Either[ParseError,AtomTerm] = {
    for{ t <- parsedToEither(fastparse.parse(in,TermwareCombinators.atomTerm(_)))
         ta <-t.kind match {
           case k: AtomTermKind =>
             Right(k.atomTerm(k.pointTerm(t)))
           case other =>
             Left(ParseError(s"atom exprected",SourceLocation(sourceName,0,0)))
         }
       } yield ta
  }

  private def check[T](f: MultiTerm => Option[T])(t:MultiTerm,tp:String):Either[ParseError,T] = {
    f(t).toRight(ParseError(s"$tp is expected",SourceLocation(in.fname(),0,0)))
  }

  private def parsedToEither(parsed:Parsed[MultiTerm]):Either[ParseError,MultiTerm] = {
    parsed match {
      case Parsed.Success(t,index) => Right(mapSourceLocation(t))
      case Parsed.Failure(label,index,extra) => Left(ParseError(label,in.markTo(index)))
    }
  }

  private def mapSourceLocation(term: MultiTerm): MultiTerm = {
     term.kind match {
       case EmptyTerm.Kind => term
       case AndSetTerm.Kind => AndSetTerm.Kind.andSet(term).mapReduce(mapSourceLocation _)(_ and _)(StarTerm.U)
       case other =>
         // TODO: implement.
         term
     }
  }


}

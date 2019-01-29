package termware.util

import java.io.Reader

import termware.ufastparse.{IndexedParserInput, StringBasedIndexedParserInput, TermwareFastParser}
import termware._

case class ParseError(
    message: String,
    sourceLocation: SourceLocation
)


trait TermwareParser {

  type Position


  def sourceName: String

  def readTerm(): Either[ParseError, MultiTerm]

  def readPrimitive[T](ops: PrimitiveTermOps[T]): Either[ParseError,PrimitiveTerm[T]]

  def readAtom(): Either[ParseError,AtomTerm]

}

object TermwareParser
{

  def fromString(input: String, fname: String = "<string>"): TermwareParser = {
      val indexedInput = new StringBasedIndexedParserInput(input,fname)
      new TermwareFastParser(indexedInput)
  }

}

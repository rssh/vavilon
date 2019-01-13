package termware

import org.scalatest.FunSpec
import termware.ufastparse.TermwareParser

class ParserSpec extends FunSpec {

  it("parse long primitives") {
    import fastparse._
    import fastparse.NoWhitespace._
    val p1 = parse("1223",ufastparse.TermwareParser.primitiveTerm(_))
    assert(p1.isSuccess)
    p1 match {
      case Parsed.Success(t,index) =>
         System.err.println("readed: "+t)
         assert(t.isInstanceOf[PrimitiveTerm[_]])
         val pt = t.asInstanceOf[PrimitiveTerm[Long]]
         assert(pt.value == 1223L)
         val source = pt.resolve(AtomTerm("source"))
         System.err.println("Source: "+source)
      case Parsed.Failure(s,index,extra) =>
         System.err.println("Failure: "+s)
    }
  }

}

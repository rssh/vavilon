package termware

import org.scalatest.FunSpec

class ParserSpec extends FunSpec {

  it("parse long primitives") {
    val p1 = TermwareParser.primitiveTerm.unapply("1223")
    assert(p1.isDefined)
    val t = p1.get.value
    System.err.println("readed: "+t)
    assert(t.isInstanceOf[PrimitiveTerm[_]])
    val pt = t.asInstanceOf[PrimitiveTerm[Long]]
    assert(pt.value == 1223L)
    val source = pt.resolve(AtomTerm("source"))
  }

}

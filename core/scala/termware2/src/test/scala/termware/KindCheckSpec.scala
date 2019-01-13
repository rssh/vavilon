package termware

import org.scalatest.FunSpec
import termware.util.NameIndexed


class KindCheckSpec extends FunSpec {

  def mtKindAnalysis(x:MultiTerm):String = {
    x.kind match {
      case pk:PointTermKind => "P" + ptKindAnalysis(pk.pointTerm(x))
      case ek:EmptyTermKind => "E"
      case sk:StarTermKind => "*"
      case ok:OrSetTermKind => "O"
      case ak:AndSetTermKind => "A"
      case mk:OrElseTermKind => "|"
      case gk:IfTermKind => "?"
    }
  }

  def ptKindAnalysis(x:PointTerm):String = {
    x.kind match {
      case a: PrimitiveTermKind => "P"
      case a: AtomTermKind => "A"
      case s: SingletonNameKind => "S"
      case x: ArrowTermKind => "->"
      case s: StructuredTermKind => "F"
    }
  }

  it("Check kind of all terms") {
    assert(mtKindAnalysis(BooleanTermBase(true))=="PP")
    assert(mtKindAnalysis(EmptyTerm)=="E")
    assert(mtKindAnalysis(StarTerm.U)=="*")
    assert(mtKindAnalysis(ArrowTerm(EmptyTerm,EmptyTerm))=="P->")
    assert(mtKindAnalysis(PlainStructuredTerm(AtomName("AAA"),
         NameIndexed.empty[MultiTerm] :+ (AtomName("A")-> AtomName("A"))
      )) =="PF")

  }

}

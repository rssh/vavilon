package termware

import org.scalatest.FunSpec


// TODO: add property-based tests
class UnificationSpec extends FunSpec {

  it("check unification of different primitives") {

    val b1 = ByteTerm(1.byteValue())
    val i1 = IntTerm(1.intValue())

    val r1 = b1 unify b1
    assert(!r1.isEmpty())

    val r2 = b1 unify i1
    assert(r2.isEmpty())

    val s1 = StringTerm("s1")
    val s2 = StringTerm("s2")
    val s11 = StringTerm("s1")

    assert((s1 unify s2).isEmpty())
    assert(!(s1 unify s11).isEmpty())

  }


  it("check unfication of star term") {
    import DSL._

    val r1 = STAR unify IntTerm(1)
    assert(!r1.isEmpty())

    assert(!(STAR unify STAR).isEmpty())

    assert((STAR unify EmptyTerm).isEmpty())

    assert((EmptyTerm unify STAR).isEmpty())

  }

  it("check unfication of structured term") {
    import DSL._

    val s1 = 'a( 'b->'b, 'c -> STAR )

    val s2 = 'a( 'q->'b,  'c-> 1)

    assert((s1 unify s2).isEmpty())

    val s3 = 'a( 'b->'b,  'c-> 1)

    val r = s1 unify s3

    assert(!r.isEmpty())

    assert(r.externalContext().termApply('c)==IntTerm(1))

  }

  it("check unfication of structured term with default values") {
    pending  //("not implemented")
  }

  }
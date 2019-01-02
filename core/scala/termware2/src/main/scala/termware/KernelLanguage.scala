package termware

import scala.language.implicitConversions

import termware.kernelLanguage.KernelContext
import termware.util.NameIndexed

object KernelLanguage {


  def evalCheck(check: PointTerm, x: MultiTerm /*, context: MultiTerm*/): PointTerm = {
    // kernel-langiage is a super-simple wher we can have:
    // - constants.
    // - equality and unification operations
    // - number relations
    // - and and or
    val r = KernelContext.applyCheck(check.addExternalContext('this --> x))
    interpretCheckAnswer(r)
  }

  def interpretCheckAnswer(x:MultiTerm): PointTerm = {
    x.kind match {
      case k: EmptyTermKind => BooleanTerm(false)
      case k: StarTermKind => BooleanTerm(true)
      case k: PointTermKind => k.pointTerm(x)
      case _: OrSetTermKind | _: AndSetTermKind => (x unify BooleanTerm.TRUE) match {
        case EmptyTerm => BooleanTerm(false)
        case other => BooleanTerm(other.externalContext().isStar())
      }
      case k: OrElseTermKind =>
        k.orElse(x).firstMapped(interpretCheckAnswer _)(_ == BooleanTerm.TRUE)(BooleanTerm.FALSE)
      case k: IfTermKind =>
        val ifTerm = k.guarded(x)
        evalCheck(ifTerm.condition,ifTerm.value)
    }
  }

  def contextWithFailure(context: MultiTerm, message: String): MultiTerm = {
    val failureArrow = ArrowTerm.apply(KernelNames.failureName,StringTerm(message))
    context or failureArrow
  }

  def Apply(f: MultiTerm, x:MultiTerm): MultiTerm = {
    PlainStructuredTerm(KernelNames.applyName,NameIndexed.fromSeq(Seq(
      AtomName("f") -> f,
      AtomName("x") -> x
    )))
  }

  def And(x:MultiTerm, y:MultiTerm): PointTerm = {
    PlainStructuredTerm(KernelNames.andName,NameIndexed.fromSeq(Seq(
      AtomName("x") -> x,
      AtomName("y") -> y
    )))
  }


  def Or(x:MultiTerm, y:MultiTerm): PointTerm = {
    PlainStructuredTerm(KernelNames.orName,NameIndexed.fromSeq(Seq(
      AtomName("x") -> x,
      AtomName("y") -> y
    )))
  }

  def atom(s:String) = AtomTerm(s)

  implicit def symbolToAtom(symbol:Symbol): AtomTerm = AtomTerm(symbol.name)

  @inline final def star = StarTerm.U
  @inline final def STAR = StarTerm.U

  type BooleanTerm = PrimitiveTerm[Boolean]

  implicit def booleanToBooleanTerm(value: Boolean): BooleanTerm = BooleanTerm(value)


}

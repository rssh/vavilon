package termware

import termware.util.NameIndexed

object KernelLanguage {


  def evalCheck(check: MultiTerm, x: MultiTerm, context: MultiTerm): TermInContext = ???

  def constantEvelBoolean(check: MultiTerm, x: MultiTerm, context: MultiTerm): Option[Boolean] = ???

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

  def And(x:MultiTerm, y:MultiTerm): MultiTerm = {
    PlainStructuredTerm(KernelNames.andName,NameIndexed.fromSeq(Seq(
      AtomName("x") -> x,
      AtomName("y") -> y
    )))

  }

}

package termware

/**
  * contradiction (i.e. $\bot$ term
  */
class ContradictionTerm(override val context: MultiTerm) extends MultiTerm
{

  override def kind: MultiTermKind = ContradictionTerm

  override def apply(term: PointTerm): MultiTerm = this

  override def resolve(term: MultiTerm): MultiTerm = this

  override def resolved(): MultiTerm = this

  override def unify(term: MultiTerm): MultiTerm = this

  override def contextMerge(otherContext: MultiTerm): MultiTerm = {
    if (otherContext.isContradiction()) {
      // TODO:  summarize all error messages ?
      otherContext
    } else {
      this
    }
  }

  override def subst(context: MultiTerm): MultiTerm = this

}


object ContradictionTerm extends ContradictionTermKind
{
  override def contradiction(x: MultiTerm): ContradictionTerm = {
    x.asInstanceOf[ContradictionTerm]
  }
}
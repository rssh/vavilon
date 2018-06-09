package termware

/**
  * contradiction (i.e. $\bot$ term
  */
class ContradictionTerm(context: MultiTerm) extends MultiTerm
{

  override def kind: MultiTermKind = ContradictionTerm

  override def apply(term: PointTerm): MultiTerm = this

  override def resolve(term: MultiTerm): MultiTerm = this

  override def resolved(): MultiTerm = this

}


object ContradictionTerm extends ContradictionTermKind
{
  override def contradiction(x: MultiTerm): ContradictionTerm = {
    x.asInstanceOf[ContradictionTerm]
  }
}
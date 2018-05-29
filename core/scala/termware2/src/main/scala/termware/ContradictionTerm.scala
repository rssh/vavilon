package termware

/**
  * contradiction (i.e. $\bot$ term
  */
class ContradictionTerm(context: MultiTerm) extends MultiTerm
{

  override def kind: MultiTermKind = ContradictionTerm
}


object ContradictionTerm extends ContradictionTermKind
{
  override def contradiction(x: MultiTerm): ContradictionTerm = {
    x.asInstanceOf[ContradictionTerm]
  }
}
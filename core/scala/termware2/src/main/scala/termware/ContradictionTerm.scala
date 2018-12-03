package termware

import termware.util.FastRefOption

/**
  * contradiction (i.e. $\bot$ term
  */
/*
class ContradictionTerm(context: MultiTerm) extends MultiTerm
{

  override def kind: MultiTermKind = ContradictionTerm

  override def apply(term: PointTerm): MultiTerm = this

  override def resolve(term: MultiTerm): MultiTerm = this

  override def resolved(): MultiTerm = this

  override def unify(x: MultiTerm): MultiTerm =
    this

  override def subst(context: MultiTerm): MultiTerm = this

  override def and(x: MultiTerm): MultiTerm = this

  override def or(x:MultiTerm):MultiTerm = this

}


object ContradictionTerm extends ContradictionTermKind
{

  def createWithContex(context: MultiTerm = EmptyTerm): ContradictionTerm = {
    new ContradictionTerm(context)
  }

  override def contradiction(x: MultiTerm): ContradictionTerm = {
    x.asInstanceOf[ContradictionTerm]
  }

}

object IsContradictionTerm
{

  def unapply(x:MultiTerm): FastRefOption[ContradictionTerm] = {
    x.kind match {
      case xk:ContradictionTermKind =>
        new FastRefOption(xk.cast(x))
      case _ =>
        FastRefOption(null)
    }
  }

}
*/
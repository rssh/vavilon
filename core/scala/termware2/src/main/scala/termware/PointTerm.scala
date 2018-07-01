package termware

trait PointTerm extends MultiTerm {

  def name: Name

  def arity: Int

  def context(): MultiTerm

  override def kind: PointTermKind

  override lazy val resolved: MultiTerm = resolve(this)

  override def resolve(term: MultiTerm): MultiTerm =
    term.kind match {
      case x: PointTermKind => apply(x.pointTerm(term))
      case x: EmptyTermKind => EmptyTerm
      case x: StarTermKind => EmptyTerm
      case x: ContradictionTermKind => term
    }

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  override def unify(term: MultiTerm): MultiTerm = {
    term.kind match {
      case x: EmptyTermKind => term
      case s: StarTermKind =>
               val checkExpression = s.cast(term).resolve(KernelNames.checkName)
               if (checkExpression.isEmpty()) {
                 this
               } else {
                 if (KernelLanguage.evalCondition(checkExpression)) {
                   this
                 } else {
                   EmptyTerm
                 }
               }
      case c: ContradictionTermKind => term
      case pt: PointTermKind => pointUnify(pt.pointTerm(term))
    }
  }


  def pointUnify(term:PointTerm):MultiTerm

  def contextMerge(y: MultiTerm): MultiTerm = (this unify y).resolved()

}

object PointTerm
{

}



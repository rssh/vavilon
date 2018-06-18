package termware


case class ArrowTerm(left: MultiTerm, right: MultiTerm) extends PointTerm
{
  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm

  override def context(): MultiTerm = left.context()

  override def apply(term: PointTerm): MultiTerm = {
    val unification = left.unify(term)
    if (unification.isEmpty() || unification.isContradiction()) {
      EmptyTerm
    } else {
      right.subst(left.unify(term).context())
    }
  }

  override def pointUnify(term: PointTerm): MultiTerm = {
    term.kind match {
      case x: ArrowTermKind => arrowUnify(x.cast(term))
      case _ => EmptyTerm
    }
  }

  def arrowUnify(other: ArrowTerm): MultiTerm = {
    map2(_ <> other.left,
      (right,nleft) => (right <> other.right).ifExists(nleft).subst(nleft.context)
    )
  }

  override def subst(context: MultiTerm): MultiTerm = map(_.subst(context))

  def map(f:MultiTerm=>MultiTerm):MultiTerm=
    map2(f,(r,_)=>f(r))

  def map2(fLeft:MultiTerm => MultiTerm,
      fRight:(MultiTerm,MultiTerm)=>MultiTerm):MultiTerm =
   {
     val nLeft = fLeft(left)
     if (nLeft.isEmpty()) {
       EmptyTerm
     } else {
       val nRight = fRight(right,nLeft)
       if (nRight.isEmpty()) {
         EmptyTerm
       } else {
         ArrowTerm(nLeft,nRight)
       }
     }
   }

}



object ArrowTerm extends ArrowTermKind {
  override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
  override def cast(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]


}

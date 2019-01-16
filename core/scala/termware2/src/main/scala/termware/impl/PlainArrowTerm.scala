package termware.impl

import termware._

case class PlainArrowTerm(left: MultiTerm, right: MultiTerm) extends ArrowTerm with PointTermNoExternalContext
{

  override def termApply(term: PointTerm): MultiTerm = {
    val u = left.unify(term)
    if (u.isExists()) {
      right.subst(u.externalContext())
    } else {
      u
    }
  }


  override def pointUnify(plt: PointTermKind, u: PointTerm):MultiTerm = {
    plt match {
      case k: ArrowTermKind => arrowUnify(k,k.arrow(u))
      case _ => EmptyTerm
    }
  }

  def arrowUnify(k:ArrowTermKind, x: ArrowTerm): MultiTerm = {
    val lu = left unify x.left
    if (!lu.isEmpty()) {
      val ec = lu.externalContext()
      val commonLeft = lu.dropExternalContext().subst(ec)
      val rightThis = right.subst(ec)
      val rightX = x.right.subst(ec)
      val ru = rightThis unify rightX
      if (ru.externalContext().isEmpty()) {
        // then result of applying to left and right will be the same.
        AndSetTerm.createPoints(this,x)
      } else {
        // TODO:  Log  (term with histroy)
        EmptyTerm
      }
    } else {
      AndSetTerm.createPoints(this,x)
    }
  }

  override def subst(context: MultiTerm): MultiTerm = map(_.subst(context))

  override def resolve(term: MultiTerm): MultiTerm = left.resolve(term)

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    PlainArrowTerm(left.pushInternalContext(context),right)
  }

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


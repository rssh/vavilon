package termware

import termware.util.FastRefOption

trait ArrowTerm extends PointTerm
{

  def left: MultiTerm

  def right: MultiTerm

  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm.Kind

}

case class PlainArrowTerm(left: MultiTerm, right: MultiTerm) extends ArrowTerm with PointTermNoExternalContext
{

  override def apply(term: PointTerm): MultiTerm = {
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



object ArrowTerm  {

  def apply(left: MultiTerm, right: MultiTerm): MultiTerm = {
    // TODO: refine.
    new PlainArrowTerm(left,right)
  }

  object Kind extends ArrowTermKind {
    override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
    override def cast(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
  }

}

object IsArrowTerm
{

  def unapply(arg: MultiTerm): FastRefOption[ArrowTerm] = {
    arg.kind match {
      case kt:ArrowTermKind => new FastRefOption[ArrowTerm](kt.arrow(kt.pointTerm(arg)))
      case _ => FastRefOption.empty
    }
  }

}
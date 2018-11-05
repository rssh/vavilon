package termware

import termware.util.FastRefOption


case class ArrowTerm(left: MultiTerm, right: MultiTerm) extends PointTerm
{
  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm

  override def apply(term: PointTerm): MultiTerm = {
    val u = left.unify(TermInContext(term,EmptyTerm))
    if (u.term.isExists()) {
      right.subst(u.context)
    } else {
      u.term
    }
  }


  override def pointUnify(plt: PointTermKind, x: PointTermInContext): TermInContext = {
    plt match {
      case k: ArrowTermKind => arrowUnify(k,x)
      case _ => TermInContext.empty
    }
  }

  def arrowUnify(k:ArrowTermKind, x: TermInContext): TermInContext = {
    val ax = k.arrow(k.pointTerm(x.term))
    val lu = left unify (ax.left ^^ x.context)
    if (!lu.term.isExists()) {
      lu
    } else {
      val ru = right unify (ax.right ^^ lu.context)
      if (!ru.term.isExists()) {
        ru
      } else {
        ArrowTerm(lu.term,ru.term) ^^ ru.context
      }
    }
  }

  override def subst(context: MultiTerm): MultiTerm = map(_.subst(context))

  override def resolve(term: MultiTerm): MultiTerm = left.resolve(term)

  override def pointAnd(ptk:PointTermKind, x:PointTerm):MultiTerm = {
    x match {
      case IsArrowTerm(ax) =>
        val lu = (left <> (ax.left ^^ EmptyTerm ))
        if (lu.term.isEmpty()) {
          SetTerm.create(this,ax)
        } else {
          val ru = right.unify(ax.right ^^ lu.context)
          if (ru.term.isEmpty()) {
            ContradictionTerm.createWithContex(SetTerm.fromMap(Map(
              AtomName("msg") -> StringTerm("unification mismatch"),
              AtomName("x") -> ax.right,
              AtomName("y") -> right
            )))
          } else {
            ru.term
          }
        }
      case _ => EmptyTerm
    }
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



object ArrowTerm extends ArrowTermKind {
  override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
  override def cast(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
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
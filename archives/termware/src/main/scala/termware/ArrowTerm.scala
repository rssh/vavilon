package termware

trait ArrowTerm extends PointTerm
{
  def left: MultiTerm
  def right: MultiTerm

  override def name = ArrowName
  override def arity: Int = 0

  override def pointKind: PointKind = PointKind.Arrow(this)

  override def uncontext: ArrowTerm with EmptyContext

  override def orPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Arrow(a) =>
        val joinLeft = left and a.left
        if (joinLeft.isEmpty) {
          SetTerm.create(this,a)
        } else {
          if (joinLeft == left) {
            SeqOrTerm.create(
              ArrowTerm.create(joinLeft, right or a.right),
              ArrowTerm.create(a.left,a.right)
            )
          } else if (joinLeft == a.left) {
            SeqOrTerm.create(
              ArrowTerm.create(joinLeft, right or a.right),
              ArrowTerm.create(left,right)
            )
          } else {
            SeqOrTerm.create(
              ArrowTerm.create(joinLeft, right or a.right),
              SetTerm.create(this,other)
            )
          }
        }
      case _ => SetTerm.create(this,other)
    }

  override def andPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Arrow(a) =>
        val joinLeft = left and a.left
        if (joinLeft.isEmpty) {
          EmptyTerm
          //SetTerm.create(this,other)
        } else {
          val joinRight = (right and a.right)
          if (joinRight.isEmpty) {
            ContextContradictionTerm(
              SetTerm.create(
                ArrowTerm.create(StringTerm("message"),StringTerm("empty common right")),
                ArrowTerm.create("left",left),
                ArrowTerm.create("right",right)
              )
            )
          } else {
            ArrowTerm.create(joinLeft,joinRight)
          }
        }
      case _ => EmptyTerm
    }

  override def pointUnify(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Arrow(otherArrow) =>
          //
           val nLeft = left unify otherArrow.left
           val nRight = right unify otherArrow.right
           (nLeft ~> nRight) in (nLeft.context and nRight.context)
      case _ => EmptyTerm
    }

  override def subst(x: MultiTerm): MultiTerm = ???



}

object ArrowTerm
{

  def create(left:MultiTerm, right:MultiTerm):MultiTerm =
  {
    left.multiKind match {
      case MultiKind.Empty(e) => right
      case MultiKind.Contradiction(e) => e
      case MultiKind.Point(p) =>
                         // TODO: grab common context ???
                         DefaultArrowTerm(left,right)
      case MultiKind.Set(s)  => s.mapOr(x => create(x,right))
                          //
      case MultiKind.SeqOr(s) => DefaultArrowTerm(left,right)
      case MultiKind.Star(s) => DefaultArrowTerm(left,right)
    }
  }

}


case class DefaultArrowTerm(val left: MultiTerm, right: MultiTerm) extends ArrowTerm with EmptyContext
{


  override type Self = DefaultArrowTerm

  override lazy val nameTerm = DefaultArrowTerm(left.nameTerm,right.nameTerm)

  override def uncontext: DefaultArrowTerm = this

  override def in(ctx: MultiTerm): MultiTerm = ContextArrowTerm.create(this,ctx)

  override def resolve(x: PointTerm): MultiTerm = EmptyTerm

  override def apply(x:MultiTerm): MultiTerm = {
    (left unify x).multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Contradiction(ct) => ct
      case other => right.subst(other.x.context)
    }
  }

  override def subst(x: MultiTerm): MultiTerm =
     new DefaultArrowTerm(left.subst(x), right.subst(x))


  override def check(x: PointTerm): Boolean =
    left.unify(KernelNames.checkNameTerm).multiKind match {
      case MultiKind.Empty(e) => true
      case MultiKind.Contradiction(x) => false
      case MultiKind.Star(s) =>
          val ev = KernelLanguage.evaluator().run(right.subst(s.context),x)
          ScalaCompability.existsTrue(ev)
      case MultiKind.Set(s) =>
          val rset = s.mapOr(e => right.subst(e.context))
          ScalaCompability.existsTrue(rset)
      case MultiKind.SeqOr(s) =>
        s.find(_.check(x)).multiKind match {
          case MultiKind.Empty(e) => false
          case MultiKind.Contradiction(ct) => false
          case MultiKind.Point(pt) => ScalaCompability.asBoolean(pt)
          case _ => false
        }
      case MultiKind.Point(pt) =>
        ScalaCompability.existsTrue(KernelLanguage.eval(pt.apply(x)))
    }
 }


object ContextArrowTerm
{

  def create(a: ArrowTerm,context:MultiTerm):MultiTerm =
      context.multiKind match {
        case MultiKind.Empty(e) => a
        case MultiKind.Contradiction(e) => e
        case MultiKind.Star(s) => create(a, s.context)
        case _ =>
          // TODO: call check before crrate ?
          DefaultArrowTerm(a.left.in(context),a.right.in(context))
      }

}


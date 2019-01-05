package termware

trait AtomTerm extends PointTerm
{
  def arity = 0

  override def nameTerm = this

  override def uncontext: AtomTerm with EmptyContext

  override def pointKind: PointKind = PointKind.Atom(this)

  override def orPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Atom(a) if this.name == a.name => other
      case _ => SetTerm.create(this,other)
    }

  override def andPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Atom(a) if this.name == a.name => this
      case _ => EmptyTerm
    }

  override def subst(x: MultiTerm): MultiTerm = {
    x.resolve(this).multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Contradiction(x) => this
      case other => other.x
    }
  }

  override def apply(other: MultiTerm): MultiTerm =
    other.multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Contradiction(ct) => ct
      case _ => EmptyTerm   // TODO: think abotu contradiction ???
    }


}


case class DefaultAtomTerm(val name:Name) extends AtomTerm with EmptyContext
{

  override type Self = DefaultAtomTerm

  override def in(ctx: MultiTerm): PointTerm =
                                     new ContextAtomTerm(this,ctx)

  override def uncontext(): AtomTerm with EmptyContext = this

  override def pointUnify(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Atom(otherAtom) => if (otherAtom.name == name) otherAtom else EmptyTerm
       case _ => EmptyTerm
     }

  override def check(x: PointTerm): Boolean = false

}

case class ContextAtomTerm(origin:AtomTerm with EmptyContext, override val context: MultiTerm) extends PointTerm with AtomTerm
{

  type Self = ContextAtomTerm

  override def arity: Int = 0

  override def uncontext: AtomTerm with EmptyContext = origin

  override def pointUnify(other: PointTerm): MultiTerm =
    {
      context.resolve(this).multiKind match {
        case MultiKind.Empty(e) =>
          other.pointKind match {
            case PointKind.Atom(a) => if (a.name == name) a else EmptyTerm
            case _ => EmptyTerm
          }
        case MultiKind.Contradiction(c) => c
        case resolved => val r = other.in(resolved.x.context)
           r.unify(other) in (this ~> r)
      }
    }

  /**
    * narrow x to be compatible with this as context
    * law:  check(narrow(x)) = true.
    */
  override def narrow(x: PointTerm): PointTerm = ???

  /**
    * true, if <code>x</code> is compatible with <code>this</code> as context.
    *
    * @param x
    * @return
    */
  override def check(x: PointTerm): Boolean = ???

  override def updateContext(ctx: MultiTerm): Unit = ???
}

object ContextAtomTerm
{
  def create(a:AtomTerm,ctx:MultiTerm): MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => a
      case MultiKind.Contradiction(e) => e
      case _ =>
        val newContext = (a.context and ctx)
        newContext.multiKind match {
          case MultiKind.Contradiction(c) => c
          case MultiKind.Empty(e) => a
          case _ => new ContextAtomTerm(a.uncontext,newContext)
        }
    }
}
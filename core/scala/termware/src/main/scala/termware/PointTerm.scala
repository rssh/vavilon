package termware

trait PointTerm extends MultiTerm {

  def arity: Int

  def pointKind: PointKind

  override def uncontext: PointTerm with EmptyContext

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Point(this)

  override def in(context: MultiTerm): MultiTerm

  def inside(context: MultiTerm): MultiTerm

  /**
    * Context from PointTerm means nothing.
    */
  override def check(x: PointTerm): Boolean = true

  override def unify(other:MultiTerm): MultiTerm =
    other.multiKind match {
      case MultiKind.Contradiction(e) => e
      case MultiKind.Empty(e) => e
      case MultiKind.Star(s) => other.in(context)
      case MultiKind.Set(s) => s.mapOr(pointUnify)
      case MultiKind.SeqOr(s) => unify(s.head).multiKind match {
        case MultiKind.Contradiction(c) => unify(s.tail)
        case MultiKind.Empty(e) => unify(s.tail)
        case x => x.x
      }
      case MultiKind.Point(p) => pointUnify(p)
    }

  def pointUnify(other:PointTerm): MultiTerm

  def orPoint(other:PointTerm):MultiTerm

  def andPoint(other:PointTerm):MultiTerm


  override def or(other:MultiTerm):MultiTerm =
   other.multiKind match {
     case MultiKind.Contradiction(e) => e
     case MultiKind.Empty(e) => this
     case MultiKind.Set(s) => s.mapOr(this.or)
     case MultiKind.SeqOr(s) => SeqOrTerm.create(this or s.head, this or s.tail)
     case MultiKind.Star(s) => s
     case MultiKind.Point(p) => orPoint(p)
   }

  override def and(other:MultiTerm):MultiTerm =
    other.multiKind match {
      case MultiKind.Contradiction(e) => e
      case MultiKind.Empty(e) => e
      case MultiKind.Set(s) => s.mapOr(this.or)
      case MultiKind.SeqOr(s) => SeqOrTerm.create(this and s.head, this and s.tail)
      case MultiKind.Star(s) => s
      case MultiKind.Point(p) => orPoint(p)
    }


}


trait PointTermImpl[S <: PointTermImpl[S]] extends PointTerm with MultiTermImpl[S] {

  this : S =>

}



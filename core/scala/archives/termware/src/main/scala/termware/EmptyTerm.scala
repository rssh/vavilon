package termware

trait EmptyTerm extends MultiTerm with EmptyContext


case object EmptyTerm extends EmptyTerm {

  override def name: Name = EmptyName

  override def nameTerm = KernelNames.emptyNameTerm

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Empty(this)

  override def context: MultiTerm = this

  override def in(ctx: MultiTerm): MultiTerm = this

  override def inside(ctx: MultiTerm): MultiTerm = this

  override def uncontext: EmptyTerm = this

  override def or(other: MultiTerm): MultiTerm = other

  override def and(other: MultiTerm): MultiTerm = other

  override def apply(other: MultiTerm): MultiTerm = this

  override def unify(x: MultiTerm): MultiTerm = this

  override def subst(x: MultiTerm): MultiTerm = this

  override def check(x: PointTerm): Boolean = true
}

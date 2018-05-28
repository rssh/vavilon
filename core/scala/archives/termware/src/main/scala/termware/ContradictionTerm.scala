package termware

import scala.collection.immutable.Queue


trait ContradictionTerm extends MultiTerm {

  override def name: Name = ContradictionName

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Contradiction(this)

  override def check(x: PointTerm): Boolean = false

}


case class ContextContradictionTerm(context:MultiTerm) extends ContradictionTerm
{

  override lazy val nameTerm = ContextAtomTerm(KernelNames.contradictionNameTerm,context)

  override def in(ctx: MultiTerm): MultiTerm = ???

  override def inside(ctx: MultiTerm): MultiTerm = in(ctx)


  override def uncontext: MultiTerm with EmptyContext = ContextlessContraditionTerm

  override def path: Queue[PointTerm] = ???

  override def resolve(x: PointTerm): MultiTerm = ???

  override def narrow(x: PointTerm): PointTerm = ???

  override def or(other: MultiTerm): MultiTerm = this

  override def and(other: MultiTerm): MultiTerm = this

  // (*) is not applicable directly.
  // (todo: rethink)
  override def apply(other: MultiTerm): MultiTerm = EmptyTerm

  // TODO: rethink
  override def unify(x: MultiTerm): MultiTerm = this

  override def subst(x: MultiTerm): MultiTerm = this

}

object ContextContradictionTerm
{

  def create(nvs:Tuple2[Name,MultiTerm]*):ContextContradictionTerm =
  {
    val ctx = SetTerm.create(nvs.map{case (n,v) => ArrowTerm.create(DefaultAtomTerm(n),v)}:_*)
    ContextContradictionTerm(ctx)
  }

  def withMessages(nvs:Tuple2[String,String]*):ContextContradictionTerm =
    create(nvs.map{case (n,v) => (AtomName(n),StringTerm(v))}: _*)

}

case object ContextlessContraditionTerm extends ContradictionTerm with EmptyContext
{

  override def nameTerm = KernelNames.contradictionNameTerm

  override def in(ctx: MultiTerm): MultiTerm = ContextContradictionTerm(ctx)

  override def resolve(x: PointTerm): MultiTerm = ???

  override def or(other: MultiTerm): MultiTerm = this

  override def and(other: MultiTerm): MultiTerm = this

  override def apply(other: MultiTerm): MultiTerm = this

  override def unify(x: MultiTerm): MultiTerm = this

  override def subst(x: MultiTerm): MultiTerm = this

  override def inside(ctx: MultiTerm): MultiTerm =
     ContextContradictionTerm.apply(ctx)

}

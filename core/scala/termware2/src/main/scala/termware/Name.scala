package termware


sealed trait Name extends Ordered[Name] with PointTerm
{

  type Carrier

  def typeIndex: Int

  def compare(that: Name): Int =
  {
    val cmp = typeIndex - that.typeIndex
    if (cmp !=0) cmp else compareSameTypeIndex(that)
  }

  def compareSameTypeIndex(that: Name): Int

  def carrier: Carrier

  override def name = this

  override def arity = 0


}

abstract class StringLikeName(val value:String) extends Name
{

   type Carrier = String

   override def carrier: String = value

   def compareSameTypeIndex(that: Name) =
     value compare that.asInstanceOf[StringLikeName].value

}


@specialized(Byte,Int,Long,Double,Char)
trait PrimitiveName[T] extends Name
{

  def value: T

  def ops: PrimitiveTermOps[_,T]

  override type Carrier = T

  override def carrier: Carrier = value

  override def typeIndex: Int = ops.primitiveTypeIndex

  def compareSameTypeIndex(that: Name) =
    ops.ordering.compare(value, that.asInstanceOf[PrimitiveName[T]].value)


}

/**
  * Singleton name, each name have unique type-index. (example: Universum)
  * @param typeIndex
  */
abstract class SingletonName(override val typeIndex: Int) extends Name with SingletonNameKind
{
  override final type Carrier = Unit

  override final def kind: PointTermKind = this

  override final def carrier: Carrier = ()

  override final def compareSameTypeIndex(that: Name): Int = 0

  override final def singletonName: SingletonName = this

  override final def pointUnify(term: PointTerm): MultiTerm = {
    term.kind match {
      case x: SingletonNameKind =>
          if (x.cast(term).typeIndex == typeIndex) {
            this
          } else {
            EmptyTerm
          }
      case _ => EmptyTerm
    }
  }

  // names are not depends from context.
  override def context(): MultiTerm = EmptyTerm

  override def subst(context: MultiTerm): MultiTerm = {
    val r = context.resolve(this)
    if (r.isEmpty() || r.isContradiction()) {
      this
    } else {
      r
    }
  }



}

final case class AtomName(s:String) extends StringLikeName(s) with AtomTerm
{

  override def kind = AtomName

  override def name: AtomName = this

  override def typeIndex: Int = TypeIndexes.ATOM

  def context(): MultiTerm = EmptyTerm


}

object AtomName extends AtomTermKind
{
  override def atomTerm(x: PointTerm): AtomTerm = x.asInstanceOf[AtomName]

}

final object SeqName extends SingletonName(TypeIndexes.SEQ)
final object SetName extends SingletonName(TypeIndexes.SET)
final object StarName extends SingletonName(TypeIndexes.STAR)
final object UnitName extends SingletonName(TypeIndexes.UNIT)
final object ContradictionName extends SingletonName(TypeIndexes.ERROR)
final object OrElseName extends SingletonName(TypeIndexes.OR_ELSE)
final object ArrowName extends SingletonName(TypeIndexes.ARROW)
final object EmptyName extends SingletonName(TypeIndexes.ARROW)


object TypeIndexes
{

  // 1-128: reserved for primitives [see primitive descriptor ]
  final val ATOM = 129
  final val EMPTY = 130

  final val SEQ  = 200
  final val SET  = 201
  final val UNIT = 202
  final val ARROW = 203
  final val OR_ELSE = 204


  final val STAR = 254
  final val ERROR =255

}


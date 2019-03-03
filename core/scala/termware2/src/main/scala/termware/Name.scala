package termware


trait NameOps extends PointTermOps with ContextCarrierTerm
{

  this: Name =>

  type Carrier

  def typeIndex: Int

  def compare(that: Name): Int =
  {
    val cmp = typeIndex - that.typeIndex
    if (cmp !=0) cmp else compareSameTypeIndex(that)
  }

  def compareSameTypeIndex(that: Name): Int

  def carrier: Carrier

  //override def name: this.type  = this

  override def arity = 0

  @inline final def toTerm(): PointTerm = this

}


trait StringLikeName extends NameOps
{

   this: Name =>

   type Carrier = String

   def value: String

   override def carrier: String = value

   def compareSameTypeIndex(that: Name) =
     value compare that.asInstanceOf[StringLikeName].value

}


@specialized(Byte,Int,Long,Double,Char)
trait PrimitiveNameOps[T] extends NameOps
{

  this: PrimitiveName[T] =>

  def value: T

  def ops: PrimitiveTermOps[T]

  override type Carrier = T

  override def carrier: Carrier = value

  override def typeIndex: Int = ops.primitiveTypeIndex

  def compareSameTypeIndex(that: Name) =
    ops.ordering.compare(value, that.asInstanceOf[PrimitiveName[T]].value)

}



trait SingletonNameOps extends NameOps
{
  this: SingletonName  =>

  override def name() = this

  def baseSingletonName(): BaseSingletonName

}



abstract class BaseSingletonName(override val typeIndex: Int) extends SingletonName with SingletonNameKind with PointTermNoExternalContext {


  override final type Carrier = Unit

  override final def kind: PointTermKind = this

  override final def carrier: Carrier = ()

  override final def compareSameTypeIndex(that: Name): Int = 0

  override final def baseSingletonName: BaseSingletonName = this

  override final def pointUnify(pk: PointTermKind, term: PointTerm): MultiTerm = {
    term.kind match {
      case x: SingletonNameKind =>
        if (x.cast(term).typeIndex == typeIndex) {
          term
        } else {
          EmptyTerm
          //TODO: add context ?  KernelLanguage.contextWithFailure(ct.context,"singleton index mismatch"))
        }
      case _ => EmptyTerm
      //  TODO: add mismatch context ?  KernelLanguage.contextWithFailure(ct.context,"name mismatch"))
    }
  }

  // names are not depends from context.
  override def context(): MultiTerm = EmptyTerm

  override def subst(context: MultiTerm): MultiTerm = {
    val r = context.resolve(this)
    if (r.isEmpty()) {
      this
    } else {
      r
    }
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new SingletonNameInInternalContext(this,context)
  }



}



class ContextfullSingletonName(term: BaseSingletonName, internContext: MultiTerm, externContext:MultiTerm) extends TermInContexts(term,internContext,externContext) with SingletonName with SingletonNameKind
{

  this: PointTerm =>

  override type Carrier = Unit

  override def typeIndex: Int = baseSingletonName.typeIndex

  override def compareSameTypeIndex(that: Name): Int = 0

  override def carrier: Carrier = term.carrier

  override def baseSingletonName(): BaseSingletonName = term

  override def kind: PointTermKind = term.kind

  override def pointUnify(ptk: PointTermKind, u: PointTerm): MultiTerm = {
    term.pointUnify(ptk,u)
  }

  override def context(): MultiTerm = internContext

  override def dropExternalContext(): PointTerm with NoExternalContext = {
    if (internContext.isEmpty()) {
      term
    } else {
      new SingletonNameInInternalContext(term, internContext)
    }
  }

}

object ContextfullSingletonName
{

  def apply(term: BaseSingletonName, internContext: MultiTerm, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      if (internContext.isEmpty()) {
        term
      } else {
        new SingletonNameInInternalContext(term,internContext)
      }
    } else {
      new ContextfullSingletonName(term, internContext, externContext)
    }
  }

}


class SingletonNameInInternalContext(term: BaseSingletonName, internContext: MultiTerm) extends SingletonName with SingletonNameKind  with PointTermNoExternalContext {

  override type Carrier = Unit

  override def typeIndex: Int = term.typeIndex

  override def compareSameTypeIndex(that: Name): Int = 0

  override def carrier: Carrier = ()

  override def baseSingletonName(): BaseSingletonName = term

  override def context(): MultiTerm = internContext

  override def kind: PointTermKind = term.kind

  override def pointUnify(ptk: PointTermKind, u: PointTerm): MultiTerm = term.pointUnify(ptk,u)

  override def subst(context: MultiTerm): MultiTerm = {
    val substContext = this.context().subst(context)
    if (substContext.isEmpty()) {
       term.subst(context)
    } else {
       term.subst(context).pushInternalContext(substContext)
    }
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
     new SingletonNameInInternalContext(term, thisContext orElse context)
  }

}


final object SeqName extends BaseSingletonName(TypeIndexes.SEQ)
final object SetName extends BaseSingletonName(TypeIndexes.SET)
final object StarName extends BaseSingletonName(TypeIndexes.STAR)
final object UnitName extends BaseSingletonName(TypeIndexes.UNIT)
final object ContradictionName extends BaseSingletonName(TypeIndexes.ERROR)
final object OrElseName extends BaseSingletonName(TypeIndexes.OR_ELSE)
final object ArrowName extends BaseSingletonName(TypeIndexes.ARROW)
final object EmptyName extends BaseSingletonName(TypeIndexes.ARROW)
final object IfName extends BaseSingletonName(TypeIndexes.IF)


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
  final val IF = 205


  final val STAR = 254
  final val ERROR =255

}


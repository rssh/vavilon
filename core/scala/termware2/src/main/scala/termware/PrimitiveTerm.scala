package termware

import cats.effect.IO
import termware.util.{FastRefBooleanOption, FastRefOption}

import scala.reflect.runtime.universe

trait PrimitiveTerm[T] extends PointTerm with PrimitiveName[T]
{
  type Value = T

  def value:T

  def primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): PrimitiveTerm[T]

  def valueAs[S]:S = value.asInstanceOf[S]

  override def arity: Int = 0

  override def subst(context: MultiTerm): MultiTerm = {
     context.termApply(this)
  }

  override def pointUnify(ptk: PointTermKind, term: PointTerm): MultiTerm = {
    term.kind match {
      case k: PrimitiveTermKind =>
        val pt = k.cast(term)
        if (pt.primitiveTypeIndex == primitiveTypeIndex &&
          value == pt.value.asInstanceOf[T]
        ) {
          term
        } else {
          EmptyTerm
          // TODO: find way to trace
          //InContext(EmptyTerm,KernelLanguage.contextWithFailure(ct.context,"value mismatch"))
        }
      case _ =>
        //InContext(EmptyTerm,KernelLanguage.contextWithFailure(ct.context,"value type mismatch"))
        EmptyTerm
    }
  }

  /**
    * Term without any contexts
    * @return
    */
  def base: BasePrimitiveTerm[T]

}



trait BasePrimitiveTerm[T] extends PrimitiveTerm[T] with PrimitiveName[T] with PointTermNoExternalContext
{

  override def kind: PrimitiveTermKind = PrimitiveTerm.Kind

  override def name: Name = this

  override def termConstructor(x:T): BasePrimitiveTerm[T]

  override def context():MultiTerm = EmptyTerm

  override def pushInternalContext(context: MultiTerm): PrimitiveTerm[T] = {
    if (context.isEmpty()) {
      this
    } else {
      new PrimitiveTermInInternalContextOnly[T](this, context)
    }
  }

  override def base = this

}

class PrimitiveTermInInternalContextOnly[T](term: BasePrimitiveTerm[T], internContext: MultiTerm) extends TermInInternalContextOnly(term, internContext) with PrimitiveTerm[T] with ContextCarrierTerm with PointTermNoExternalContext
{
  override def value: T = term.value

  override def primitiveTypeIndex: Int = term.primitiveTypeIndex

  override def ordering: Ordering[T] = term.ordering

  override def termConstructor(x: T): PrimitiveTerm[T] =
    new PrimitiveTermInInternalContextOnly(term.termConstructor(x),internContext)

  override def name: Name = term.name

  override def kind: PointTermKind = term.kind

  override def context(): MultiTerm = internContext

  override def pushInternalContext(context: MultiTerm): PrimitiveTerm[T] = {
    if (context.isEmpty()) {
      this
    } else {
      new PrimitiveTermInInternalContextOnly[T](term, context orElse internContext)
    }
  }

  override def ops = term.ops

  override def base: BasePrimitiveTerm[T] = term


}

object PrimitiveTermInInternalContextOnly
{

  def apply[T](term: PrimitiveTerm[T], internContext: MultiTerm): PrimitiveTerm[T] = {
    if (internContext.isEmpty()) {
      term
    } else if (term.context().isEmpty()) {
      new PrimitiveTermInInternalContextOnly(term.base, internContext)
    } else {
      new PrimitiveTermInInternalContextOnly(term.base, internContext orElse term.context())
    }
  }

}

class ContextfullPrimitiveTerm[T](term: BasePrimitiveTerm[T], internContext: MultiTerm, externContext:MultiTerm) extends TermInContexts(term,internContext,externContext) with PrimitiveTerm[T] {

  override def value: T = term.value

  override def primitiveTypeIndex: Int = term.primitiveTypeIndex

  override def ordering: Ordering[T] = term.ordering

  override def termConstructor(x: T): PrimitiveTerm[T] = {
    // TODO: think
    term.termConstructor(x)
  }

  override def name: Name = term.name

  override def kind: PointTermKind = PrimitiveTerm.Kind

  override def context(): MultiTerm = internContext

  override def dropExternalContext(): PrimitiveTerm[T] with NoExternalContext = {
    if (internContext.isEmpty()) {
      term
    } else {
      new PrimitiveTermInInternalContextOnly[T](term, internContext)
    }
  }

  override def ops = term.ops

  override def base = term

}

object ContextfullPrimitiveTerm {

  def apply[T](term: BasePrimitiveTerm[T], internContext: MultiTerm, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      if (internContext.isEmpty()) {
        term
      } else {
        PrimitiveTermInInternalContextOnly[T](term, internContext)
      }
    } else {
      new ContextfullPrimitiveTerm(term, internContext, externContext)
    }
  }

}

object PrimitiveTerm
{

  object Kind extends PrimitiveTermKind {
    override def primitive(x: PointTerm): PrimitiveTerm[_] = {
      x.asInstanceOf[PrimitiveTerm[_]]
    }
  }


}


abstract class BasePrimitiveTermImpl[S<:BasePrimitiveTermImpl[S,T],T](val value: T) extends BasePrimitiveTerm[T] {

  this: S =>

  type Self = BasePrimitiveTermImpl[S,T]

  def ops: PrimitiveTermOps[T]

  def primitiveTypeIndex: Int = ops.primitiveTypeIndex

  def ordering: Ordering[T] = ops.ordering

  def termConstructor(x:T): BasePrimitiveTerm[T] = ops.termConstructor(x)



}


abstract class PrimitiveTermOps[T]
{

  final type Primitive = T
  type PName = PrimitiveName[T]

  val primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): BasePrimitiveTerm[T]

}


case class ByteTermBase(v:Byte) extends BasePrimitiveTermImpl[ByteTermBase,Byte](v)
{

  override val ops: PrimitiveTermOps[Byte] = ByteTermOps

}

object ByteTermOps extends PrimitiveTermOps[Byte]
{

  override final val primitiveTypeIndex: Int = 1

  override def ordering: Ordering[Byte] = implicitly[Ordering[Byte]]

  override def termConstructor(x: Byte): ByteTermBase = ByteTermBase(x)

}

case class ShortTermBase(v:Short) extends BasePrimitiveTermImpl[ShortTermBase,Short](v)
{
  def ops = ShortTermOps
}

object ShortTermOps extends PrimitiveTermOps[Short]
{
  override final val primitiveTypeIndex: Int = 2

  override def ordering: Ordering[Short] = implicitly[Ordering[Short]]

  override def termConstructor(x: Short): ShortTermBase = ShortTermBase(x)

}

case class IntTermBase(v:Int) extends BasePrimitiveTermImpl[IntTermBase,Int](v)
{
  def ops = IntTermOps
}

object IntTermOps extends PrimitiveTermOps[Int]
{
  override final val primitiveTypeIndex: Int = 3

  override def ordering: Ordering[Int] = implicitly[Ordering[Int]]

  override def termConstructor(x: Int): IntTermBase = IntTermBase(x)
}

case class LongTermBase(v:Long) extends BasePrimitiveTermImpl[LongTermBase,Long](v)
{
  def ops = LongTermOps
}

object IntTerm extends (Int => BasePrimitiveTerm[Int])
{
  @inline
  override def apply(v: Int): BasePrimitiveTerm[Int] = IntTermBase(v)
}


object LongTermOps extends PrimitiveTermOps[Long]
{
  override final val primitiveTypeIndex: Int = 4

  override def ordering: Ordering[Long] = implicitly[Ordering[Long]]

  override def termConstructor(x: Long): LongTermBase = LongTermBase(x)
}

object LongTerm extends (Long => BasePrimitiveTerm[Long])
{
  @inline
  override def apply(v: Long): BasePrimitiveTerm[Long] = LongTermBase(v)
}

case class DoubleTermBase(v:Double) extends BasePrimitiveTermImpl[DoubleTermBase,Double](v)
{
  def ops = DoubleTermOps
}

object DoubleTermBase
{
  implicit def ops = DoubleTermOps
}

object DoubleTermOps extends PrimitiveTermOps[Double]
{
  override val primitiveTypeIndex: Int = 8

  override def ordering: Ordering[Double] = implicitly[Ordering[Double]]

  override def termConstructor(x: Double): DoubleTermBase = DoubleTermBase(x)
}

object DoubleTerm extends (Double => BasePrimitiveTerm[Double])
{
  @inline
  override def apply(v: Double): BasePrimitiveTerm[Double] = DoubleTermBase(v)
}


case class BigDecimalTermBase(v:BigDecimal) extends BasePrimitiveTermImpl[BigDecimalTermBase,BigDecimal](v)
{
  def ops = BigDecimalTermOps
}

object BigDecimalTermOps extends PrimitiveTermOps[BigDecimal]
{
  override val primitiveTypeIndex: Int = 9

  override def ordering: Ordering[BigDecimal] = implicitly[Ordering[BigDecimal]]

  override def termConstructor(x: BigDecimal) = BigDecimalTermBase(x)
}

object BigDecimalTerm extends (BigDecimal => PrimitiveTerm[BigDecimal])
{
  @inline
  override def apply(v: BigDecimal): BasePrimitiveTerm[BigDecimal] = BigDecimalTermBase(v)

}



//TODO: add unsigned types


case class CharTermBase(v:Char) extends BasePrimitiveTermImpl[CharTermBase,Char](v)
{
  def ops = CharTermOps
}


object CharTermOps extends PrimitiveTermOps[Char]
{
  override final val primitiveTypeIndex: Int = 17

  override def ordering: Ordering[Char] = implicitly[Ordering[Char]]

  override def termConstructor(x: Char): CharTermBase = CharTermBase(x)
}

object CharTerm extends (Char => PrimitiveTerm[Char])
{
  @inline
  override def apply(v: Char): BasePrimitiveTerm[Char] = CharTermBase(v)

  implicit def ops: PrimitiveTermOps[Char] = CharTermOps

}


case class StringTermBase(v:String) extends BasePrimitiveTermImpl[StringTermBase,String](v)
{
  def ops = StringTermOps
}


object StringTermOps extends PrimitiveTermOps[String]
{
  override final val primitiveTypeIndex: Int = 18

  override def ordering: Ordering[String] = implicitly[Ordering[String]]

  override def termConstructor(x: String) = StringTermBase(x)
}

object StringTerm extends (String => BasePrimitiveTerm[String])
{
  override def apply(v: String): BasePrimitiveTerm[String] = StringTermBase(v)

}


case class OpaqueTermBase(v:Array[Byte]) extends BasePrimitiveTermImpl[OpaqueTermBase,Array[Byte]](v)
{
  def ops = OpaqueTermOps
}


object OpaqueTermOps extends PrimitiveTermOps[Array[Byte]]
{

  override final val primitiveTypeIndex: Int = 19

  override val ordering: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) => {
    val c0 = x.length - y.length
    if (c0 != 0) {
      c0
    } else {
      var i = 0
      var c = 0
      while (i < x.length && c != 0) {
        c = x(i) - y(i)
        i += 1
      }
      c
    }
  }

  override def termConstructor(x: Array[Byte]) = OpaqueTermBase(x)
}

object OpaqueTerm extends (Array[Byte] => PrimitiveTerm[Array[Byte]])
{
  override def apply(v: Array[Byte]): BasePrimitiveTerm[Array[Byte]] = OpaqueTermBase(v)
}


case class BooleanTermBase(v:Boolean) extends BasePrimitiveTermImpl[BooleanTermBase,Boolean](v)
{
  def ops = BooleanTermOps

}

object BooleanTermOps extends PrimitiveTermOps[Boolean]
{
  override val primitiveTypeIndex: Int = 20

  override def ordering: Ordering[Boolean] = implicitly[Ordering[Boolean]]

  override def termConstructor(x: Boolean): BasePrimitiveTerm[Boolean] = BooleanTermBase(x)
}

object BooleanTerm extends (Boolean => PrimitiveTerm[Boolean])
{
  @inline
  override def apply(v: Boolean): PrimitiveTerm[Boolean] = BooleanTermBase(v)

  def unapply(v: MultiTerm): FastRefBooleanOption = {
     v.kind match {
       case k: PrimitiveTermKind =>
         val pv = k.primitive(k.pointTerm(v))
         if (pv.primitiveTypeIndex == BooleanTermOps.primitiveTypeIndex) {
           FastRefBooleanOption.fromBoolean(pv.valueAs[Boolean])
         } else {
           FastRefBooleanOption.Empty
         }
       case _ => FastRefBooleanOption.Empty
     }
  }

  final val TRUE = BooleanTerm(true)
  final val FALSE = BooleanTerm(false)

}


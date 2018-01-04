package termware

import cats.kernel.CommutativeMonoid
import cats.{Applicative, CommutativeApplicative, Eval, Traverse, UnorderedTraverse}

/**
  * Set of names and values (i.e. named record)
  * where fields have names and order.
  * (i.e. { x[index=0](0), y[index=1](0) } mean record with two fields,
  *  first is 'x', second is 'y', Point(1,2), Point(x=1,y=2), Point(y=2,x=1) mean the same)
  */
case class NameIndexed[+T](nameIndexes:Map[Name,Int],values: IndexedSeq[T])
{

  def :+[S >:T](p: Tuple2[Name,S]):NameIndexed[S] =
    NameIndexed(nameIndexes.updated(p._1,values.size), values :+ p._2)

  def updated[S >: T](n:Name,v:S):NameIndexed[S] =
     this :+ (n,v)

  def size = values.size

  def get(n:Name):Option[T] = nameIndexes.get(n) map values

  def get(i:Int):Option[T] = if (i>=0 && i < values.size) Some(values(i)) else None

  lazy val names = nameIndexes.toIndexedSeq.sortBy(_._2).map(_._1)

  def  map[S](f:T=>S):NameIndexed[S] = {
    NameIndexed(nameIndexes,values.map(f))
  }

  def  exists(p: T=>Boolean): Boolean = {
    values.exists(p)
  }

  def foldLeft[S](s0:S)(f:(S,(Name,T))=>S):S =
  {
    nameIndexes.foldLeft(s0){ case (s,(n,i)) =>
      f(s,(n,values(i)))
    }
  }

  def foldRight[S](s0:S)(f:((Name,T),S)=>S):S =
  {
    nameIndexes.foldRight(s0){ case ((n,i),s) =>
      f((n,values(i)),s)
    }
  }

  def foldWhile[S](s0:S)(p: S => Boolean)(f: (S,(Name,T))=>S): S =
  {
    var s = s0
    nameIndexes.find { case (n,i) =>
      s=f(s,(n,values(i)))
      ! p(s)
    }
    s
  }


}


object NameIndexed
{
  def empty[V] = NameIndexed[V](Map(),IndexedSeq())

  def fromSeq[V](x:Seq[(Name,V)]): NameIndexed[V] = {
    x.foldLeft(empty[V]){ _ :+ _ }
  }

  def fromMap[V](x:Map[Name,V]):NameIndexed[V] = {
    x.foldLeft(empty[V]){ _ :+ _ }
  }

  def pair[V](n:Name,v:V): NameIndexed[V] = empty[V] :+ (n,v)


  implicit object NITraverse extends Traverse[NameIndexed] {
    import cats.syntax.all._

    override def traverse[G[_], A, B](fa: NameIndexed[A])(f: (A) => G[B])(implicit ev1: Applicative[G]): G[NameIndexed[B]] = {
       fa.map(f).foldLeft(ev1.pure(NameIndexed.empty[B])){(s,e)=>
         ev1.map2(s,e._2)( (s,v) => s.updated(e._1,v) )
       }
    }

    override def foldLeft[A, B](fa: NameIndexed[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)((s,e)=>f(s,e._2))

    override def foldRight[A, B](fa: NameIndexed[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldRight(lb)((e,s)=>f(e._2,s))

  }

}
package termware.util

import scala.language.higherKinds
import cats.{Applicative, CommutativeApplicative, Eval, Traverse, UnorderedTraverse}
import termware.util.NameIndexed.Record
import termware.{AtomName, AtomTerm, Name}

/**
  * Set of names and values (i.e. named record)
  * where fields have names and order.
  * (i.e. { x[index=0](0), y[index=1](0) } mean record with two fields,
  *  first is 'x', second is 'y', Point(1,2), Point(x=1,y=2), Point(y=2,x=1) mean the same)
  *
  * AtomContext can contains check and default values.
  */
case class NameIndexed[+T](nameIndexes:Map[AtomTerm,Int] = Map(),
                           records: IndexedSeq[NameIndexed.Record[T]] = IndexedSeq())
{

  def :+[S >:T](p: (AtomTerm,S)):NameIndexed[S] =
    NameIndexed[S](nameIndexes.updated(p._1,records.size), records :+ Record(p._1,p._2))

  def updated[S >: T](n:AtomTerm,v:S):NameIndexed[S] =
     this :+ (n,v)

  def size = records.size

  def get(n:AtomTerm):Option[T] = (nameIndexes.get(n) map records) map (_.value)

  def get(i:Int):Option[T] = if (i>=0 && i < records.size) Some(records(i).value) else None

  lazy val names = nameIndexes.toIndexedSeq.sortBy(_._2).map(_._1)

  def  map[S](f:T=>S):NameIndexed[S] = {
    NameIndexed(nameIndexes,records.map(_ map f))
  }

  def  exists(p: T=>Boolean): Boolean = {
    records.exists(r => p(r.value))
  }

  def foldLeft[S](s0:S)(f:(S,(AtomTerm,T))=>S):S =
  {
    nameIndexes.foldLeft(s0){ case (s,(n,i)) =>
      f(s,(n,records(i).value))
    }
  }

  def foldRight[S](s0:S)(f:((AtomTerm,T),S)=>S):S =
  {
    nameIndexes.foldRight(s0){ case ((n,i),s) =>
      f((n,records(i).value),s)
    }
  }

  def foldWhile[S](s0:S)(p: S => Boolean)(f: (S,(AtomTerm,T))=>S): S =
  {
    var s = s0
    nameIndexes.find { case (n,i) =>
      s=f(s,(n,records(i).value))
      ! p(s)
    }
    s
  }


}


object NameIndexed
{

  case class Record[+T](term:AtomTerm,value:T) {
    def map[S](f:T=>S):Record[S] = Record(term,f(value))
  }

  def empty[V] = NameIndexed[V](Map(),IndexedSeq())

  def fromSeq[V](x:Seq[(AtomTerm,V)]): NameIndexed[V] = {
    x.foldLeft(empty[V]){ _ :+ _ }
  }

  def fromMap[V](x:Map[AtomTerm,V]):NameIndexed[V] = {
    x.foldLeft(empty[V]){ _ :+ _ }
  }

  def pair[V](n:AtomTerm,v:V): NameIndexed[V] = empty[V] :+ (n,v)


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
package termware

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

  def size = values.size

  def get(n:Name):Option[T] = nameIndexes.get(n) map values

  def get(i:Int):Option[T] = if (i>=0 && i < values.size) Some(values(i)) else None

}


object NameIndexed
{
  def empty[V] = NameIndexed[V](Map(),IndexedSeq())

  def fromSeq[V](x:Seq[(Name,V)]): NameIndexed[V] = {
    x.foldLeft(empty[V]){ _ :+ _ }
  }

}
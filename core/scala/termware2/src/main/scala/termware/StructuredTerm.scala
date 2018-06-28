package termware

import termware.util.NameIndexed

trait StructuredTerm extends PointTerm {

  type Self <: StructuredTerm

  override def kind: StructuredTermKind = StructuredTerm

  def nameTerm: AtomTerm


  def get(name: AtomName): Option[MultiTerm]



  /**
    * @param i : 0 < i < arity
    * @return
    */
  def get(i:Int):Option[MultiTerm]

  def names(): IndexedSeq[AtomName]

  def indexedSubterms(): IndexedSeq[MultiTerm]

  def namedSubterms(): NameIndexed[MultiTerm]

  def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self

  def mapSubterms(f:MultiTerm => MultiTerm): Self

  override def subst(context: MultiTerm): MultiTerm = {
    // TODO: think about mapping head.
    // TODO: wrap in IO
    mapSubterms(_.subst(context))
  }

  override def context(): MultiTerm = nameTerm.context()

  override def pointUnify(term: PointTerm): MultiTerm = {
    term.kind match {
      case k:StructuredTermKind =>
        structuredUnify(k.cast(term))
      case _ => EmptyTerm
    }
  }

  def structuredUnify(term: StructuredTerm): MultiTerm = {
    if (arity == term.arity) {
      val unifiedName = nameTerm.unify(term.nameTerm)
      if (unifiedName.isExists()) {
        unifiedName.kind match {
          case x:AtomTermKind =>
            val newName = x.cast(x.pointTerm(unifiedName))
            val newSubterms = NameIndexed.empty[MultiTerm]
            val s0:MultiTerm = StructuredTerm.create(newName,newSubterms)
            namedSubterms().foldWhile(s0)(_.isExists()){ case (s,(n,v)) =>
              term.get(n) match {
                case None => s
                case Some(otherValue) =>
                  val u = v.contextMerge(s).unify(otherValue)
                  s.kind match {
                    case k:StructuredTermKind =>
                      k.structured(k.pointTerm(s)).updated(n,u)
                    case other => s
                  }
              }
            }
          case other => EmptyTerm
        }
      } else {
        EmptyTerm
      }
    } else {
      EmptyTerm
    }
  }

  def updated(name:AtomName, value: MultiTerm): MultiTerm

}

object StructuredTerm extends StructuredTermKind
{

  override def cast(x: PointTerm): StructuredTerm = x.asInstanceOf[StructuredTerm]

  def create(nameTerm: AtomTerm, indexes: NameIndexed[MultiTerm]): StructuredTerm =
    PlainStructuredTerm(nameTerm,indexes)

}

case class PlainStructuredTerm(override val nameTerm: AtomTerm,
                               indexes:NameIndexed[MultiTerm]) extends StructuredTerm
{
  override type Self = PlainStructuredTerm

  override def name: Name = nameTerm.name

  override def get(name: AtomName): Option[MultiTerm] = indexes.get(name)

  /**
    * @param i : 0 < i < arity
    * @return
    */
  override def get(i: Int): Option[MultiTerm] = indexes.get(i)

  override def names(): IndexedSeq[AtomName] = indexes.names

  override def indexedSubterms(): IndexedSeq[MultiTerm] = indexes.records.map(_.value)

  override def namedSubterms(): NameIndexed[MultiTerm] = indexes

  override def arity: Int = indexes.size

  override def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self =
    PlainStructuredTerm(nameTerm,newIndexes)

  override def updated(name: AtomName, value: MultiTerm): MultiTerm = {
    // TODO:  check that value satisficy context.
    PlainStructuredTerm(name,indexes.updated(name,value))
  }

  override def mapSubterms(f: MultiTerm => MultiTerm): PlainStructuredTerm = {
    // TODO: check that resilt is correct
    PlainStructuredTerm(nameTerm,indexes.map(f))
  }

}


package termware

trait EmptyTerm extends MultiTerm


case object EmptyTerm extends EmptyTerm {
  override def name: Name = EmptyName

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Empty(this)

  override def context: MultiTerm = this

  override def updateContext(ctx: MultiTerm): MultiTerm = ???

  override def uncontext: MultiTerm with EmptyContext = ???

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}


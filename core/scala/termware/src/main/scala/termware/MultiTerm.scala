package termware

trait MultiTerm {

  def name: Name
  def cardinality: Int
  def multiKind: MultiKind

  def isEmpty: Boolean  = this == EmptyTerm

  def context: MultiTerm
  def updateContext(ctx:MultiTerm): MultiTerm
  def uncontext: MultiTerm with EmptyContext

  def mergeAsLeftContext(other: MultiTerm): MultiTerm
  def mergeAsRightContext(other: MultiTerm): MultiTerm = other.mergeAsLeftContext(this)

  def selectAsContextPattern(other: MultiTerm): MultiTerm
  def selectAgainstContextPattern(other: MultiTerm): MultiTerm = other.selectAsContextPattern(this)

  def mergeAsScopeAnd(other: MultiTerm):MultiTerm
  def mergeAsScopeOr(other:MultiTerm): MultiTerm

  def selectAsLeftPattern(other:MultiTerm):MultiTerm

}



trait MultiTermImpl[S <: MultiTermImpl[S]] extends MultiTerm {

    thisMultiTerm: S =>
    //type D
    type Self = S //<: MultiTerm.Aux[D]


}





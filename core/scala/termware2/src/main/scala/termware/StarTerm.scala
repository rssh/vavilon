package termware

trait StarTerm extends MultiTerm
{

  def context: MultiTerm

}

class PlainStarTerm(val context: MultiTerm) extends StarTerm {

  override def kind: MultiTermKind = StarTerm

  override def apply(term: PointTerm): MultiTerm =
                                context.resolve(term)

  override def resolve(term: MultiTerm): MultiTerm =
                                EmptyTerm

  override def resolved(): MultiTerm = this

}


object StarTerm extends StarTermKind {

  val U = new PlainStarTerm(EmptyTerm)


}




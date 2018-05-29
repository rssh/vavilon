package termware

trait StarTerm extends MultiTerm
{

  def context: MultiTerm

}

class PlainStarTerm(val context: MultiTerm) extends StarTerm {

  override def kind: MultiTermKind = StarTerm

}


object StarTerm extends StarTermKind {

  val U = new PlainStarTerm(EmptyTerm)

  override def star(x:MultiTerm): StarTerm =
    x.asInstanceOf[StarTerm]

}




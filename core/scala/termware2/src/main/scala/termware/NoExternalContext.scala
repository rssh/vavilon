package termware

trait NoExternalContext extends MultiTerm {


  override def externalContext(): MultiTerm = StarTerm.U

  override def dropExternalContext(): MultiTerm with NoExternalContext = this

  override def setExternalContext(extrnContext: MultiTerm): MultiTerm = {
      TermInExternalContext(this,extrnContext)
  }

  def pushContext(): MultiTerm = this

}

trait PointTermNoExternalContext extends PointTerm with NoExternalContext
{

  override def dropExternalContext(): PointTerm with NoExternalContext = this

}
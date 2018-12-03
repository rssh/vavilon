package termware

trait NoExternalContext extends MultiTerm {

  override def externalContext(): MultiTerm = StarTerm.U

  override def dropExternalContext(): MultiTerm = this

  override def setExternalContext(extrnContext: MultiTerm): MultiTerm = {
      TermInExternalContext(this,extrnContext)
  }

  def pushContext(): MultiTerm = this

}

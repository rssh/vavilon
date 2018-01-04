package termware

object KernelNames {

  val termwareName = AtomName("termware")
  val termwareNameTerm = termwareName.toTerm

  val termwareContext = ArrowTerm.create(nameTerm,termwareNameTerm)

  val kernelName = AtomName("kernel")
  val kernelNameTerm = kernelName.toTerm



  lazy val kernelContext = ContextArrowTerm.create(DefaultArrowTerm(nameTerm,kernelNameTerm),termwareContext)

  lazy val name = AtomName("name")
  lazy val nameTerm: PointTerm = name.toTerm.in(kernelContext)



  val seqName = AtomName("[]")
  val seqNameTerm = seqName.toTerm.in(kernelContext)

  val structuredName = AtomName("<<>>")
  val structuredNameTerm = structuredName.toTerm.in(kernelContext)

  val setName = AtomName("{}")
  val setNameTerm = setName.toTerm.in(kernelContext)

  val starName = AtomName("*")
  val starNameTerm = starName.toTerm.in(kernelContext)

  val unitName = AtomName("()")
  val unitNameTerm = unitName.toTerm.in(kernelContext)

  val contradictionName = AtomName("_|_")
  val contradictionNameTerm = contradictionName.toTerm

  val arrowName = AtomName("->")
  val arrowNameTerm = arrowName.toTerm.in(kernelContext)

  val orElseName = AtomName("|")
  val orElseNameTerm = orElseName.toTerm.in(kernelContext)

  val emptyName = AtomName("(0)")
  val emptyNameTerm = emptyName.toTerm.in(kernelContext)

  val checkName = AtomName("check")
  val checkNameTerm = checkName.toTerm.in(kernelContext)

}

package termware

import org.scalatest.{FunSpec, FunSuite}


class BaseMultiTermTest extends FunSuite {

 test("primitive term must be created and joined to set") {

  val intTerm = IntTerm(5)

  val stringTerm = StringTerm("5")

  val setTerm = DefaultSetTerm(Seq(intTerm,stringTerm))

  assert(setTerm.isContradiction == false)
 }



}

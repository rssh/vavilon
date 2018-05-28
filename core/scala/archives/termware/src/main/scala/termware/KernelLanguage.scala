package termware

import termware.strategies.EarlyTopDown

object KernelLanguage {

  def eval(x:MultiTerm):MultiTerm =
   evaluator().run(ruleset(),x)

  def ruleset(): MultiTerm = ???

  def evaluator():EvaluationStrategy =
     EarlyTopDown

//  implicit def pairToAtom(x:Tuple2[PointTerm,MultiTerm]) : MultiTerm = {
//    DefaultArrowTerm(x._1,x._2)
//  }

  class KernelTermSyntax(val value: MultiTerm) extends AnyVal
  {




  }

}

package termware

import termware.strategies.EarlyTopDown

object KernelLanguage {


  def evaluator():EvaluationStrategy =
     EarlyTopDown

}

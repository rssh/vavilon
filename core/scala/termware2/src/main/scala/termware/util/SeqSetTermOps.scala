package termware.util

import termware._

// TODO: implement.
//  emptyseq == EmptyTerm
// This is the most simply unoptimized form, which will be changed later.
trait SeqSetTermOps extends SetTermOps
{

  this: SetTerm =>

  val seq: IndexedSeq[MultiTerm]


  override def mapReduce[A](mapf: MultiTerm => A)(reduce: (A, A) => A)(zero: =>A):A = {
    val s0 = zero
    seq.foldLeft(s0){ (s,e) =>
      reduce(s,mapf(e))
    }
  }

  override def members(): Seq[MultiTerm] = seq

  override def resolve(term: MultiTerm): MultiTerm = {
    seq.view.map(_.resolve(term)).find(!_.isEmpty()).getOrElse(EmptyTerm)
  }


}

package termware.strategies


import scala.language.higherKinds
import cats.effect.{Effect, IO}
import cats.instances.all._
import cats.syntax.traverse._
import termware.{EmptyTerm, EvaluationStrategy, MultiKind, MultiTerm, NameIndexed, PointKind, PointTerm}


object EarlyTopDown extends EvaluationStrategy {

  override def runIO(ruleset: MultiTerm, arg: MultiTerm): Eff[MultiTerm] =
  {
    ruleset.apply(arg).multiKind match {
      case MultiKind.Empty(e) => runStepIO(ruleset,arg)
      case other => IO.pure(EvaluationResult(other.x,true))
    }
  }

  def runStepIO(ruleset: MultiTerm, arg: MultiTerm): Eff[MultiTerm] = {
    arg.multiKind match {
      case MultiKind.Empty(e) => IO.pure(EvaluationResult(e,false))
      case MultiKind.Contradiction(ct)  => IO.pure(EvaluationResult(ct,false))
      case MultiKind.Star(s) => IO.pure(EvaluationResult(s,false))
      case MultiKind.Set(s) =>
        val s0: Eff[MultiTerm] = IO.pure(EvaluationResult(EmptyTerm,false))
        s.mapReduce(s0)(runStepIO(ruleset,_)){
             (ex,ey) => ex.flatMap(x => if (x.changed) IO.pure(x) else ey )
        }
      case MultiKind.SeqOr(s) =>
        runStepIO(ruleset,s.head).flatMap(r => if (r.changed) IO.pure(r) else runStepIO(ruleset,s.tail))
      case MultiKind.Point(pt) => runPointIO(ruleset,pt)
    }
  }

  def runPointIO(ruleset: MultiTerm, arg: PointTerm): Eff[MultiTerm] = {
    arg.pointKind match {
      case PointKind.Structured(st) =>
        val s0 = IO.pure(EvaluationResult(NameIndexed.empty[MultiTerm],false))
        // TODO:  find instead fold, we don't need to reconstruct term in unsuccessful case
        st.namedSubterms().foldLeft(s0){ (s,e) =>
          s.flatMap(r => if (r.changed)
                           IO.pure(EvaluationResult(r.value :+ e, true))
                         else
                           runIO(ruleset,e._2).map(rn => EvaluationResult(
                              r.value.updated(e._1,rn.value),rn.changed))
          )
        }.map{ r =>
          r.map(v => if (r.changed) st.newNamedSubterms(v) else st)
        }
      case PointKind.Sequence(sq) => runSeqIO(ruleset,IndexedSeq(),sq.subterms()).map{ r =>
        EvaluationResult(if (r.changed) sq.newSubterms(r.value) else sq, r.changed)
      }

    }
  }

  def runSeqIO(ruleset: MultiTerm, prev:IndexedSeq[MultiTerm], seq:IndexedSeq[MultiTerm]):Eff[IndexedSeq[MultiTerm]] = {
    if (seq.isEmpty) {
      IO.pure(EvaluationResult(prev,false))
    } else {
      runIO(ruleset, seq.head).flatMap(r =>
        if (r.changed) {
          val nseq = (prev :+ r.value) ++ seq.tail
          IO.pure(EvaluationResult(nseq, true))
        } else {
          runSeqIO(ruleset, prev :+ seq.head, seq.tail)
        }
      )
    }
  }

}

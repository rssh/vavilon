package termware


import cats.Monad

import scala.annotation.tailrec





/**
  * Evaluation strategy
  * TODO: change to run in monad.
  */
trait EvaluationStrategy  {

  case class EvaluationResult[+X](value: X, changed: Boolean)
  {

    def map[B](f:X=>B):EvaluationResult[B] = EvaluationResult(f(value),changed)

    def flatMap[B](f:X=>EvaluationResult[B]) = {
      val newResult = f(value)
      EvaluationResult(newResult.value,changed || newResult.changed)
    }

  }

  object EvaluationResult
  {
    implicit object impMonad extends Monad[EvaluationResult]
    {
      override def pure[A](x: A): EvaluationResult[A] = EvaluationResult(x,false)

      //override def ap[A, B](ff: EvaluationResult[(A) => B])(fa: EvaluationResult[A]): EvaluationResult[B] = {
      //   val b = ff.value(fa.value)
      //   EvaluationResult(b,fa.changed || ff.changed)
      //}

      override def flatMap[A, B](fa: EvaluationResult[A])(f: (A) => EvaluationResult[B]): EvaluationResult[B] = {
         fa.flatMap(f)
      }

      override def tailRecM[A, B](a: A)(f: (A) => EvaluationResult[Either[A, B]]): EvaluationResult[B] =
      {
        @tailrec
        def doInternal(a:A,f: (A) => EvaluationResult[Either[A,B]], changed: Boolean): EvaluationResult[B] = {
          val fa = f(a)
          fa.value match {
            case Left(a) => doInternal(a, f, changed || fa.changed)
            case Right(b) => EvaluationResult(b,changed || fa.changed)
          }
        }
        doInternal(a,f,false)
      }


    }
  }

  type Eff[X] = IO[EvaluationResult[X]]

  def runIO(ruleset: MultiTerm, arg: MultiTerm): Eff[MultiTerm]

  def runPointIO(ruleset: MultiTerm, arg: PointTerm): Eff[MultiTerm]

  def run(ruleset: MultiTerm, arg: MultiTerm): MultiTerm =
  {
    val io = runIO(ruleset,arg)
    io.unsafeRunSync().value
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

  protected def applyOrUnchanged(ruleset: MultiTerm, arg: EvaluationResult[MultiTerm]):EvaluationResult[MultiTerm] = {
    ruleset.apply(arg.value).multiKind match {
      case MultiKind.Empty(e) => arg
      case other => EvaluationResult(other.x,true)
    }
  }


  implicit object EffEffect extends Effect[Eff]
  {

    override def runAsync[A](fa: Eff[A])(cb: (Either[Throwable, A]) => IO[Unit]): IO[Unit] =
      IO.ioEffect.runAsync[EvaluationResult[A]](fa){
        case err@Left(ex) => cb(Left(ex))
        case Right(value) => cb(Right(value.value))
      }

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Eff[A] =
      IO.ioEffect.async(k).map(EvaluationResult(_,true))

    override def suspend[A](thunk: =>Eff[A]): Eff[A] =
      IO.suspend(thunk)

    override def pure[A](x: A): Eff[A] =
      IO.pure(EvaluationResult(x,false))

    override def flatMap[A, B](fa: Eff[A])(f: (A) => Eff[B]): Eff[B] =
      IO.ioEffect.flatMap(fa) { x =>
        f(x.value).map(fx => EvaluationResult(fx.value,x.changed||fx.changed))
      }

    override def tailRecM[A, B](a: A)(f: (A) => Eff[Either[A, B]]): Eff[B] =
      IO.ioEffect.tailRecM[EvaluationResult[A],EvaluationResult[B]](EvaluationResult(a,false)){ ra =>
        f(ra.value).map{ r =>
          r.value match {
            case Left(a) => Left(EvaluationResult(a,r.changed || ra.changed))
            case Right(b) => Right(EvaluationResult(b,r.changed || ra.changed))
          }
        }
      }

    override def raiseError[A](e: Throwable): Eff[A] =
    {
      IO.ioEffect.raiseError[EvaluationResult[A]](e)
    }

    override def handleErrorWith[A](fa: Eff[A])(f: (Throwable) => Eff[A]): Eff[A] = {
      IO.ioEffect.handleErrorWith[EvaluationResult[A]](fa)(f)
    }

  }



}

object EvaluationStrategy
{




}

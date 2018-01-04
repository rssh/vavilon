package termware

import cats.{Eval, Monad}
import cats.effect.{Async, Effect, IO}
//import cats.syntax.all._

import scala.concurrent.Promise



/**
  * Evaluation strategy
  * TODO: change to run in monad.
  */
trait EvaluationStrategy  {

  case class EvaluationResult[X](value: X, changed: Boolean)
  {

    def map[B](f:X=>B):EvaluationResult[B] = EvaluationResult(f(value),changed)

  }

  type Eff[X<:MultiTerm] = IO[EvaluationResult[X]]



  def runIO(ruleset: MultiTerm, arg: MultiTerm): Eff[MultiTerm]

  def run(ruleset: MultiTerm, arg: MultiTerm): MultiTerm =
  {
    val io = runIO(ruleset,arg)
    io.unsafeRunSync().value
  }

  /*
  implicit object EffEffect extends Effect[Eff]
  {

    override def runAsync[A](fa: Eff[A])(cb: (Either[Throwable, A]) => IO[Unit]): IO[Unit] =
      IO.ioEffect.runAsync(fa){
        case err@Left(ex) => cb(err)
        case Right(value) => cb(value.value)
      }

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Eff[A] =
      IO.ioEffect.async(k).map(EvaluationResult(_,true))

    override def suspend[A](thunk: =>Eff[A]): Eff[A] = ???

    override def pure[A](x: A): Eff[A] = ???

    override def flatMap[A, B](fa: Eff[A])(f: (A) => Eff[B]): Eff[B] = ???

    override def tailRecM[A, B](a: A)(f: (A) => Eff[Either[A, B]]): Eff[B] = ???

    override def raiseError[A](e: Throwable): Eff[A] = ???

    override def handleErrorWith[A](fa: Eff[A])(f: (Throwable) => Eff[A]): Eff[A] = ???
  }
  */

}

object EvaluationStrategy
{




}

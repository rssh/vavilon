package termware.kernelLanguage

import termware._

object KernelContext {

   import termware.KernelLanguage._




   def applyCheck(t:MultiTerm):MultiTerm = {
     val u = kernelRules.unify(t)
     u.kind match {
       case k: EmptyTermKind => EmptyTerm
       case other =>
         val check = u.resolve(KernelNames.checkName)
         if (check.isEmpty()) {
           EmptyTerm
         } else {
           u.unify(check).substExternalContext()
         }
     }
   }

  val andRules = OrSetTerm.create(
    ( 'x --> true and 'y --> true ) :- true,
    ( 'y --> false ) :- false,
    ( 'x --> false ) :- false
  )

  val orRules = OrSetTerm.create(
    ( 'x --> true ) :- true,
    ( 'y --> true ) :- true,
    ( 'x --> false and 'y --> false) :- false
  )

  def kernelRules: MultiTerm = {
     And('x,'y) ^ ('x --> STAR and 'y --> STAR and KernelNames.checkName --> andRules)
  }

}

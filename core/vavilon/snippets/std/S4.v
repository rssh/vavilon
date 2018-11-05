

// S4


syntax x:Nat = x@{ check -> (checkType Nat)@typeRules }

typeRules = {
  ((checkType x -> (a -> ! unify(a,x).term.isEmpty))@(a->*))(@x->*)
}

syntax: f* a:A -> b = f* -> (a->b)@(a->A)

typeRules = {
  checkType x:* a:* -> ! unify(a,x).term.isEmpty
}

Nat -> { 0, S(*:Nat) }

plus {
  0 x:Nat -> x,
  x:Nat 0 -> x
  x:Nat S(y:Nat) -> S(plus x y)
}

  


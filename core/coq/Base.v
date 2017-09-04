Require Import Bool.
Require Import Arith.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import FSets.FMapAVL.
Require Import Coq.Lists.ListSet.
Require Import Coq.Structures.OrderedTypeEx.

Set Implicit Arguments.

Import ListNotations.



Inductive PrimitiveValue := 
   NatTerm: nat -> PrimitiveValue
 | BooleanTerm: bool -> PrimitiveValue
 | StringTerm: string -> PrimitiveValue .


Lemma PrimitiveValue_eq_dec: forall x y:PrimitiveValue, { x = y} + { x <> y}.
 decide equality.
apply eq_nat_dec.
apply bool_dec.
apply string_dec.
Defined.

Fixpoint PrimitiveValue_eqb (x:PrimitiveValue) (y :PrimitiveValue): bool :=
  if (PrimitiveValue_eq_dec x y) then true else false
.


Inductive Name : Set := 
   PrimitiveName: PrimitiveValue-> Name
 | StringName: string -> Name
 | SeqName  .


Lemma Name_eq_dec: forall x y: Name, { x = y} + { x <> y}.
 decide equality.
 apply PrimitiveValue_eq_dec.
 apply string_dec.
Defined.


(* Module Map_Name := FMapAVL.Make Name. *)


Inductive PointTerm:Type := 
    primitiveTerm : PrimitiveValue -> PointTerm
  |  atomTerm:  Name -> PointTerm
  |  sequenceTerm:  (list MultiTerm) -> PointTerm
  |  structuredTerm: (list (Name * MultiTerm)) -> PointTerm
  |  scopedPointTerm: MultiTerm -> PointTerm -> PointTerm
  |  arrowTerm: MultiTerm -> MultiTerm -> PointTerm
  |  nilTerm
with MultiTerm:Type :=
   point: PointTerm -> MultiTerm
 | setTerm:  (set PointTerm) -> MultiTerm
 | scopedStarTerm: MultiTerm -> MultiTerm 
 | starTerm: MultiTerm
 | errorTerm: string -> MultiTerm
.

Lemma pair_eq_dec :
  forall (A B:Type) (* should be Type in coq >= V8.1 *)
         (eqA:forall a a0:A, {a=a0}+{a<>a0})
         (eqB:forall b b0:B, {b=b0}+{b<>b0})
         (x y:A*B), {x=y}+{x<>y}.
Proof. intros until 2; decide equality; auto. Qed.


Definition EmptyTerm := setTerm nil.

Lemma Atom_eq_dec: forall (nx: Name)(ny: Name), {(atomTerm nx)=(atomTerm ny)} + {(atomTerm nx)<>(atomTerm ny)}.
 intros.
assert ( {nx = ny} + {nx <> ny}).
apply Name_eq_dec.
destruct H as [|e'].
- rewrite e.
  left.
  congruence.
- right.
  congruence.
Qed.

Lemma Primitive_eq_dec: forall(px:PrimitiveValue)(py:PrimitiveValue), {(primitiveTerm px)=(primitiveTerm py)} + {(primitiveTerm px)<>(primitiveTerm py)}.
intros.
assert ({px = py} + {px <> py}).
apply PrimitiveValue_eq_dec.
destruct H as [|ne].
  left; congruence.
  right; congruence.
Qed.

Definition AtomTerm (x:Name)  := point (atomTerm x).
Definition SequenceTerm (x: list MultiTerm) := point (sequenceTerm x).


Lemma  seq_dec_eq_prop: forall x y, ( {x=y} + { x<>y } ) -> { (sequenceTerm x) = (sequenceTerm y)} + { (sequenceTerm x) <>  (sequenceTerm y)}.
intros.
case H as [E|NE].
left; congruence.
right; congruence.
Qed.

Lemma fin_list_nonrec: forall A (l: list A) (a:A), l<>a::l.
 induction l.
 - discriminate.
 - injection 1.
intros H0 _; now destruct (IHl a).
Qed.

Lemma  mt_pt_dec_prop: (forall x y: MultiTerm, { x = y} + { x <> y}) -> forall z v:PointTerm, { z = v } + { z <> v }.
Proof.
decide equality.
apply PrimitiveValue_eq_dec.
apply Name_eq_dec.
apply list_eq_dec.
apply X.
apply list_eq_dec.
apply pair_eq_dec.
apply Name_eq_dec.
apply X.
Qed.

Lemma  pt_mt_dec_prop: (forall x y: PointTerm, { x = y} + { x <> y}) -> forall z v:MultiTerm, { z = v } + { z <> v }.

Proof.
decide equality.
apply list_eq_dec.
apply X.
apply string_dec.
Qed.

Print PointTerm_ind.


Fixpoint PointTermDepth (x:PointTerm): nat :=
  match x with 
    primitiveTerm v  => 1
  |  atomTerm n => 1
  | sequenceTerm ls  =>  (fold_left (fun s e => 
                                                            (max s (MultiTermDepth e))) ls 1) + 1 
  | structuredTerm ls => (fold_left ( fun s e =>
                                                          ( max s (MultiTermDepth (snd e) ))  ) ls 1 ) + 1
 | scopedPointTerm mt pt => (max (MultiTermDepth mt) (PointTermDepth pt)  ) + 1
 | arrowTerm mx my => (max  (MultiTermDepth mx) (MultiTermDepth my)  ) + 1 
 | nilTerm => 0
  end with MultiTermDepth (x: MultiTerm): nat :=
   match x with
     point y => (PointTermDepth y) + 1
   | setTerm sp => (fold_left (fun s e => max s (PointTermDepth e)) sp 1 )+1
   | scopedStarTerm mt => (MultiTermDepth mt)+1
   | starTerm => 0
   | errorTerm s => 0
   end. 

Lemma  nat_plus1_neq0: forall x: nat, x+1 <> 0.
Proof.
 intuition.
Qed.

Lemma PointTermDepth_0:  forall x:PointTerm, (PointTermDepth x) = 0 -> x = nilTerm.
 intros.
 induction (x).
- compute in H.
 + discriminate (H).
- compute in H.
 + discriminate H.
- unfold  PointTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- unfold  PointTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- unfold  PointTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- unfold  PointTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- reflexivity.
 Qed.


Lemma MultiTermDepth_0:  forall x:MultiTerm, (MultiTermDepth x) = 0 -> { x = starTerm } + { exists s:string, x = (errorTerm s) }  .
 intros.
 remember x as y.
 induction y.
- unfold  MultiTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- compute in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- unfold  MultiTermDepth in H.
  apply nat_plus1_neq0 in H.
  destruct H.
- unfold  MultiTermDepth in H.
  auto.
- right.
  exists s.
  congruence.
Qed.

Lemma MaxAA: forall a: nat, (max a a) = a.
intro.
unfold max.
induction a.
auto.
congruence.
Qed.

Lemma MaxL: forall a b:nat, (max a b) >= a.
intros.
induction a.
- unfold max.
  { induction b.
    - auto.
    - auto.
  }
- induction b.
  auto.
  simpl.
  
 
Qed.

Lemma SeqDepthStep: forall (x:PointTerm) (yh: MultiTerm) (yt: list MultiTerm), x = sequenceTerm (yh::yt) -> (PointTermDepth x) > (MultiTermDepth yh).
Proof.
 intros.
 subst x.
 unfold PointTermDepth. 

Qed.  


Lemma unnorm_pointterm_eq_dec: forall x: PointTerm, forall y: PointTerm, { x = y } + {  x <> y }.
Proof.
intros.
remember (PointTermDepth x) as depth_x.
induction depth_x.
- symmetry in Heqdepth_x.
 apply PointTermDepth_0 in Heqdepth_x.
 subst x. 
 case y.
 intros; right; discriminate.
 intros; right; discriminate.
 intros; right; discriminate.
 intros; right; discriminate.
 intros; right; discriminate.
 intros; right; discriminate.
 left; congruence.
-
 case x.
 case y.
 intros.
 apply Primitive_eq_dec.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 case y.
 right; discriminate.
 intros.
 apply Atom_eq_dec.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 case y.
 right; discriminate.
 right; discriminate.
 intros.
 apply mt_pt_dec_prop.
 

 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.
 right; discriminate.




admit.
apply list_eq_dec.
apply pair_eq_dec.
apply Name_eq_dec.
apply pt_mt_dec_prop.
admit.
apply pt_mt_dec_prop.
admit.
apply pt_mt_dec_prop.
Qed.


Lemma pass_list:  (forall x y:MultiTerm, { x = y } + { x <> y}) -> forall x y: list MultiTerm, {x = y} + { x <> y}.
Proof.
intros.
decide equality.
Qed.



(*
Lemma unnorm_multiterm_eq_dec: forall x y:MultiTerm, { x = y } + {  x <> y }.
Proof.
 intros.
 case x.
Qed.
*)


Fixpoint unnorm_multiterm_eq (t: MultiTerm) (s: MultiTerm): bool  :=
 match t with 
  errorTerm msg => match s with
                                  errorTerm msg1  => if (string_dec msg msg1)then true else false
                                 | _  =>  false
                               end
  | point pt => match s with
                      point st => unnorm_pointterm_eq pt st
                      |_ => false
                    end
 | _ => false
 end with unnorm_pointterm_eq (pt:PointTerm)(st:PointTerm):bool :=
  match pt with
    primitiveTerm pv => match st with 
          primitiveTerm sv =>  if (PrimitiveValue_eq_dec pv sv) then true else false
          | _ => false
       end
    | _ => false
end.

Definition IsSetTerm :=  { x: MultiTerm | exists sx: (list PointTerm),  x =  setTerm  sx }.


Fixpoint setTermAdd (x: MultiTerm) (y:PointTerm): MultiTerm := 
match x with
  setTerm sx => match y with
                             nilTerm => x
                             | _ => if (set_In y sx) then sx else setTerm (set_add y sx)
                         end
 | point px => setTermAdd (setTerm [px]) y
end
.


Fixpoint withScope (t:MultiTerm) (s:MultiTerm) : MultiTerm :=
  match t wit
    | errorTerm msg => t 
    |  starTerm => scopedStarTerm s
    |  scopedStarTerm s1 => scopedStarTerm (ScopeAnd s s1)
    |  setTerm ts => setTerm (flat_map (fun x => setTerm withScopeP x s) ts) 
    |  point pt => point (withScopeP pt s)
  end with  withScopeP (t:PointTerm) (s: MultiTerm): PointTerm :=
      match t with 
        primitiveTerm value => t
      | _ => atomTerm (StringName "TODO")
  end with ScopeAnd (t:MultiTerm) (s:MultiTerm) : MultiTerm :=
    match t with
       | errorTerm msg => t
       | starTerm => s
       | scopedStarTerm t1 =>
          match s with
            starTerm => scopedStarTerm t1
          |  scopedStarTerm  s1 =>
                              scopedStarTerm (ScopeAnd t1 s1)
          | setTerm ss =>
                               setTerm (map (fun x => withScopeP x t1 ) ss)
          | point pt => point( withScopeP pt t1)
          | errorTerm msg => errorTerm msg
          end
       | setTerm ts => 
            setTerm (map (fun x => ScopeAndP x s) ts)
       | _ => AtomTerm (StringName "TODO") 
     end with ScopeAndP (t: PointTerm) (s: MultiTerm) : PointTerm =
      match t with
       primitiveTerm value =>  
          match s with
           starTerm => t
           | point ps  =>  if (t = ps) then t else nilTerm
           (*| _  => (atomTerm (StringName "TODO")) *)
          end
      |  _  => atomTerm (StringName "TODO")
   end
.



Definition normalize (x:MultiTerm): MultiTerm :=
 match x with
  | ErrorTerm msg => x
  | any => x
 end. 
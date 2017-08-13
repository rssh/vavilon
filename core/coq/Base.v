Require Import Bool.
Require Import Arith.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import FSets.FMapAVL.
Require Import Coq.Structures.OrderedTypeEx.

Set Implicit Arguments.

Import ListNotations.



Inductive PrimitiveValue := 
   NatTerm: nat -> PrimitiveValue
 | BooleanTerm: bool -> PrimitiveValue
 | StringTerm: string -> PrimitiveValue .


Inductive Name : Set := 
   PrimitiveName: PrimitiveValue-> Name
 | StringName: string -> Name
 | SeqName  .

(* Module Map_Name := FMapAVL.Make Name. *)



Inductive PointTerm: Set := 
    primitiveTerm : PrimitiveValue -> PointTerm
  |  atomTerm:  Name -> PointTerm
  |  sequenceTerm:  (list MultiTerm) -> PointTerm
  |  structuredTerm: (list (Name->MultiTerm)) -> PointTerm
  |  scopedPointTerm: MultiTerm -> PointTerm -> PointTerm
  |  arrowTerm: MultiTerm -> MultiTerm -> PointTerm
with MultiTerm: Set :=
   point: PointTerm -> MultiTerm
 | setTerm:  (list PointTerm) -> MultiTerm
 | scopedStarTerm: MultiTerm -> MultiTerm 
 | starTerm: MultiTerm
 | errorTerm: string -> MultiTerm
.


Definition EmptyTerm := setTerm nil
.

Definition AtomTerm (x:Name)  := point (atomTerm x).
Definition SequenceTerm (x: list MultiTerm) := point (sequenceTerm x).



Fixpoint withScope (t:MultiTerm) (s:MultiTerm) : MultiTerm :=
  match t with
    | errorTerm msg => t 
    |  starTerm => scopedStarTerm s
    |  scopedStarTerm s1 => scopedStarTerm (ScopeAnd s s1)
    |  setTerm ts => setTerm (map (fun x => withScopeP x s) ts) 
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
       | _ => AtomTerm (StringName "TODO") 
     end
. 



Definition normalize (x:MultiTerm): MultiTerm :=
 match x with
  | ErrorTerm msg => x
  | any => x
 end. 
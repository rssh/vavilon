Require Import Bool.
Require Import Arith.
Require Import Ascii.
Require Import String.
Require Import FSets.FMapAVL.
Require Import Coq.Structures.OrderedTypeEx.

Set Implicit Arguments.




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
    PrimitiveTerm : PrimitiveValue -> PointTerm
  |  AtomTerm:  Name -> PointTerm
  |  SequenceTerm:  (list MultiTerm) -> PointTerm
  |  StructuredTerm: (list (Name->MultiTerm)) -> PointTerm
with MultiTerm: Set :=
   PointTern
 | SetTerm:  (list PointTerm) -> MultiTerm.


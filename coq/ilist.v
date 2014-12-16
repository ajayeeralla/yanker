
Inductive ilist (A : Set) : nat -> Set :=
  | Nil : ilist A O
  | Cons : forall n, A -> ilist A n -> ilist A (S n).

SearchAbout plus.

Fixpoint irev_append {A n m} (l : ilist A n) (r : ilist A m) : (ilist A (n+m)) :=
 match l in ilist _ n return ilist _ (n+m) with
 | Nil => r
 | Cons n h t =>
   (match eq_sym (plus_n_Sm n m) in (_ = l) return ilist _ l with
     | eq_refl => irev_append t (Cons A m h r)
    end)
 end.

Definition eq_n_O (n:nat): (n + 0 = n) := eq_sym (plus_n_O n).
Lemma eq_n_O_O : forall (n:nat), (n + 0 + 0 = n).
Proof.
intros.
replace (n + 0) with n.
apply eq_n_O.
apply plus_n_O.
Show Proof.
Check eq_ind.
Qed.

Require Export Coq.Vectors.Vector.
Check nil.
(* End Category_theory. *)

(* Extraction Language Haskell. *)
(* Extraction "ilist.hs" irev_append. *)
Require Export Coq.Program.Equality.

Lemma irev_append_involutive : forall {A n} (l : ilist A n),
      l = (match eq_n_O_O n in (_ = n) return ilist _ n with
           | eq_refl => (irev_append (irev_append l (Nil A)) (Nil A))
          end).
Proof.
intros.
destruct l.
compute.
replace (eq_n_O_O 0) with (eq_refl 0).
reflexivity.
Focus 2.
replace (eq_n_O_O (S n)) with (eq_refl (S n)).
Check (eq_n_O_O 0).
Check (eq_refl 0).

compute.
reflexivity.

destruct (eq_n_O_O 0).
reflexivity.
 as [|(Nil A) |(Cons A m h t)]. 
destruct (eq_n_O_O n) as [|eq1].


Definition iapp {A n m} (a: ilist A n) (b: ilist A m) : (ilist A (n+m)) :=
   let c : ilist A n :=
      match eq_sym (plus_n_O n) in (_ = n) return ilist _ n with
       | eq_refl => irev_append a (Nil A)
      end in
   irev_append c b.

Lemma iapp_l_nil : forall {A n} (l : ilist A n),
           (match (eq_sym (plus_n_O n)) in (_ = n) return ilist _ n with 
            | eq_refl => iapp l (Nil A)
           end) = l.

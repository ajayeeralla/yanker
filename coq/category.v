Section Lists.
Require Import List.

Definition Concat : forall {A}, (list A) -> (list A) -> (list A) :=
     fun A => fun a => fun b => rev_append (rev a) b.
End Lists.
Check Concat.

Section Category_theory.

Structure Category : Type := mkCategory
{ Ob :> Set;
Hom : Ob -> Ob -> Set;
comp : forall {a b c}, Hom b c -> Hom a b -> Hom a c;
one : forall x:Ob, Hom x x;
Assoc : forall a b c d (f : Hom c d) (g : Hom b c) (h : Hom a b), comp f (comp g h) = comp (comp f g) h;
LeftId : forall a b (f : Hom a b), comp (one b) f = f;
RightId : forall a b (f : Hom a b), comp f (one a) = f
(* Truncated : forall a b (f g : Hom a b) (p q : f = g), p = q *)
}.

Bind Scope hom_scope with Ob.

Record Functor :=
{ From : Category;
  To : Category;
  FOb : From.(Ob) -> To.(Ob);
  FHom : forall {a b}, (From.(Hom) a b) -> (To.(Hom) (FOb a) (FOb b));
  FId : forall x:From.(Ob), FHom (From.(one) x) = To.(one) (FOb x);
  Func1 : forall a b c (f : From.(Hom) a b) (g : From.(Hom) b c),
  FHom (From.(comp) g f) = To.(comp) (FHom g) (FHom f)
}.

Check eq_sym.

Structure StrictMonoidalCategory (C : Category) : Type := mkStrictMonoidalCategory
{
  ot : (Ob C) -> (Ob C) -> (Ob C);
  otAssoc : forall {a b c}, (ot a (ot b c)) = (ot (ot a b) c);

  otH : forall {a b c d}, (Hom C a b) -> (Hom C c d) -> (Hom C (ot a c) (ot b d));
  otHAssoc : forall {a b c d e k} (f : Hom C a b) (g : Hom C c d) (h : Hom C e k),
             (otH (otH f g) h) = (match otAssoc in (_ = r) return Hom C _ r with
                                   | eq_refl =>
                                 (match otAssoc in  (_ = l) return Hom C l _ with
                                   | eq_refl => (otH f (otH g h))
                                  end)
                                 end);


  unit : Ob C;
  unitLeft : forall {a}, ot unit a = a;
  unitRight : forall {a}, ot a unit = a;
  unitHLeft : forall {a b} (f : Hom C a b), f =
    (match unitLeft in (_ = b) return Hom C a b with
     | eq_refl =>
       (match unitLeft in (_ = a) return Hom C a (ot unit b) with
         | eq_refl => otH (one C unit) f
       end)
     end);

  bifunc : forall {a b c d e k} (f : Hom C a b) (g : Hom C b c) (h : Hom C d e) (i : Hom C e k),
          comp C (otH g i) (otH f h) = otH (comp C g f) (comp C i h)
 }.
 
Variable C : Category.
Variable MC : StrictMonoidalCategory C.
Infix "x" := (otH C MC) (at level 60, right associativity).
Infix "o" := (comp C) (at level 65, right associativity).

Lemma exch_law_1 : forall {a b c d} (f : Hom C a b) (g : Hom C c d),
    ((one C b) x g) o (f x (one C c)) = f x g.
Proof.
intros.
replace ((one C b) x g o f x (one C c)) with ((one C b o f) x (g o one C c)).
replace (one C b o f) with f.
replace (g o one C c) with g.
reflexivity.
symmetry; apply RightId.
symmetry; apply LeftId.
symmetry; apply bifunc.
Qed.

Lemma exch_law_2 : forall {a b c d} (f : Hom C a b) (g : Hom C c d),
    (f x (one C d)) o ((one C a) x g) = f x g.
Proof.
intros.
replace ((f x (one C d)) o ((one C a) x g)) with ((f o (one C a)) x ((one C d) o g)).
replace (f o (one C a)) with f.
replace ((one C d) o g) with g.
reflexivity.
symmetry; apply LeftId.
symmetry; apply RightId.
symmetry; apply bifunc.
Qed.

Lemma exchange_law : forall {a b c d}
                   (f : Hom C a b) (g : Hom C c d),
    (((one C b) x g) o (f x (one C c))) =
      ((f x (one C d)) o ((one C a) x g)).
Proof.
intros.
replace (one C b x g o f x one C c) with (f x g).
replace (f x one C d o one C a x g) with (f x g).
reflexivity.
symmetry; apply exch_law_2.
symmetry; apply exch_law_1.
Qed.

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
Qed.

Lemma irev_append_involutive : forall {A n} (l : ilist A n),
      l = (match eq_n_O_O n in (_ = n) return ilist _ n with
           | eq_refl => (irev_append (irev_append l (Nil A)) (Nil A))
          end).
Proof.


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


Definition fObj := list (Ob C).
Definition fOt : fObj -> fObj -> fObj := Concat.

Definition fHom (dom:fObj) (cod:fObj) : Type :=
  

Definition freeMon := mkStrictMonoidalCategory C



Structure MonoidalFunctor : Type := {
     MFF :> Functor;

     MFMonFrom : StrictMonoidalCategory;
     MFMonFromC: (From MFF) = MFMonFrom;
  
     MFMonTo : StrictMonoidalCategory;
     MFMonToC: (To MFF) = MFMonTo;

     MFmorphic: forall {a b c d} (f : (MFMonFrom.(MHom) a b)) (g : (MFMonFrom.(MHom) c d)),
                  (MFMonTo.(Mot) (MFF.(FHom) f) (MFF.(FHom) g)) =
                  (MFF.(FHom) (MFMonFrom.(Mot) f g))
}.

End Category_theory.
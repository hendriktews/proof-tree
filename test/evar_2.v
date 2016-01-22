
Lemma a : exists(n : nat), n = n.
Proof.
  eexists.
  reflexivity.
Grab Existential Variables.
eexact 2.
Qed.


Lemma b : exists(n : nat), n = n.
Proof.
  eexists.
  reflexivity.
Unshelve.
eexact 2.
Qed.

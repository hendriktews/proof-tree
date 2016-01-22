
Lemma a : forall(P Q : Prop), P -> Q -> P /\ Q.
Proof.
  intros P Q H H0.
  split.
    give_up.
  eexact H0.
Qed.




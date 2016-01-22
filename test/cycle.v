
Lemma a : forall(P1 P2 P3 P4 : Prop), 
  P1 -> P2 -> P3 -> P4 -> P1 /\ P2 /\ P3 /\ P4.
Proof.
  intros P1 P2 P3 P4 H H0 H1 H2.
  repeat split.
        all : swap 1 2.
        all : cycle 2.
        all : revgoals.
        auto.
      auto.
    auto.
  auto.
Qed.




(* test -interactively- that unicode letters from a lot of scrips is
 * accepted in evars by prooftree.
 *)

Lemma x0 : exists(öçζ Щקض沒Ⴋ : nat), öçζ Щקض沒Ⴋ = 0.
Proof using.
  eexists.

datatype lam = VA of string
         | LM of string * lam
         | AP of lam * lam ;

val freshVariableIndex = ref 0

fun getFreshVariable v = 
    let val _ = (freshVariableIndex := (!freshVariableIndex) + 1)
        val i = (!freshVariableIndex)
        in (v ^ (Int.toString i))
    end;

fun alphaRename x s (VA(t)) = if x=t then VA(s) 
else VA(t)
  | alphaRename x s(LM(y,t)) = if x=y then LM(y,t) else 
    let val z = getFreshVariable "z" in
    LM(z,(alphaRename x s (alphaRename y z t))) end
  | alphaRename x s(AP(t1,t2))= AP((alphaRename x s t1),(alphaRename x s t2));

fun irr (VA(t)) = true
  | irr (LM(x,t)) = irr t
  | irr (AP(t1,t2)) = if irr t1 then irr t2 else false;

fun norReduce (VA(t)) = VA(t)
  | norReduce (LM(x,t)) = LM(x, norReduce t)
  | norReduce (AP(t1,t2)) = if irr t1 then norReduce (AP((norReduce t1),t2)) else norReduce (AP(t1,(norReduce t2)));

let
val x0 = "two"
val t0 = LM("f",LM("x",AP(VA("f"),AP(VA("f"),VA("x")))))
val x1 = "succ"
val t1 = LM("n",LM("f",LM("x",AP(VA("f"),AP(AP(VA("n"),VA("f")),VA("x"))))))
val x2 = "plus"
val t2 = LM("n",AP(VA("n"),VA("succ")))
val t = AP(AP(VA("plus"),VA("two")),VA("two"))
val main = AP(LM(x1,AP(LM(x2,t),t2)),t1)
in norReduce main end;
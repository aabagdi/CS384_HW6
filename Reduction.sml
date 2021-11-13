datatype lam = Var of string
         | Lam of string * lam
         | App of lam * lam ;

val freshVariableIndex = ref 0

fun getFreshVariable v = 
    let val _ = (freshVariableIndex := (!freshVariableIndex) + 1)
        val i = (!freshVariableIndex)
        in (v ^ (Int.toString i))
    end;

fun alphaRename x s (Var(t)) = if x=t then Var(s) 
else Var(t)
  | alphaRename x s(Lam(y,t)) = if x=y then Lam(y,t) else 
    let val z = getFreshVariable "z" in
    Lam(z,(alphaRename x s (alphaRename y z t))) end
  | alphaRename x s(App(t1,t2))= App((alphaRename x s t1),(alphaRename x s t2));

 
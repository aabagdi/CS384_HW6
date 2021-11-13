datatype lam = Var of string
         | Lam of string * lam
         | App of lam * lam ;

val freshVariableIndex = ref 0

fun getFreshVariable v = 
    let val _ = (freshVariableIndex := (!freshVariableIndex) + 1)
        val i = (!freshVariableIndex)
        val base = v
        in (base ^ (Int.toString i))
    end;

fun alphaRename x s Var(t) = 
    if x=t then Var(s) else Var(x)
  | alphaRename x s Lam(y,t) = Lam(x,t)
  | alphaRename x s Ap(t1,t2)= App(t1,t2);

 
datatype lam = Var of string
         | Lam of string * lam
         | App of lam * lam ;

val freshVariableIndex = ref 0

fun getFreshVariable v = 
    let val _ = (freshVariableIndex := (!freshVariableIndex) + 1)
        val i = (!freshVariableIndex)
        in (v ^ (Int.toString i))
    end;

fun alphaRename x s(Var(t)) = Var(t)
  | alphaRename x s(Lam(y,t)) = Lam(y,t)
  | alphaRename x s(App(t1,t2))= App(t1,t2);

 
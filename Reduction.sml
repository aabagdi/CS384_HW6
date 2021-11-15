datatype lam = VA of string
         | LM of string * lam
         | AP of lam * lam ;

val freshVariableIndex = ref 0

fun getFreshVariable v = 
    let val _ = (freshVariableIndex := (!freshVariableIndex) + 1)
        val i = (!freshVariableIndex)
        in (v ^ (Int.toString i))
    end;

fun alphaRename (VA(x)) s (VA(t)) = if x=t then s 
else VA(t)
  | alphaRename (VA(x)) s (LM(y,t)) = if x=y then LM(y,t) else 
    let val z = getFreshVariable "z" in
    LM(z,(alphaRename (VA(x)) s (alphaRename (VA(y)) (VA(z)) t))) end
  | alphaRename (VA(x)) s (AP(t1,t2))= AP((alphaRename (VA(x)) s t1),(alphaRename (VA(x)) s t2));

fun irr (VA(t)) = true
  | irr (LM(x,t)) = irr t
  | irr (AP(t1,t2)) = false;

fun norReduce (VA(t)) = VA(t)
  | norReduce (LM(x,t)) = LM(x, norReduce t)
  | norReduce (AP(LM(x,t),s)) = 
if irr t 
then if irr s 
     then alphaRename (VA(x)) s t 
     else
          let val News =  norReduce s in norReduce (AP(LM(x,t), News )) end 
else let val newT = norReduce (LM(x,t)) in norReduce (AP(newT, s )) end 
  |norReduce(AP(t1,t2)) = AP(t1,t2);


fun pretty (VA(x)) = x
  | pretty (LM(x,t)) = "fn "^x^" => "^(pretty t)
  | pretty (AP(t1,t2)) = "("^ (pretty t1) ^")("^(pretty t2)^")";

let
val x1 = "zero"
val t1 = LM("f",LM("x",VA("x")))
val t = VA("zero")
val main = AP(LM(x1,t),t1)
val value = norReduce main
in
	print (pretty value)
end;


import Estado

data AExp = Num Int
          |Var String
          |Som AExp AExp
          |Sub AExp AExp
          |Mul AExp AExp
          deriving(Show)

data BExp = TRUE
          | FALSE
          | Not BExp
          | And BExp BExp
          | Or  BExp BExp
          | Ig  AExp AExp
          | Leq AExp AExp
          deriving(Show)

data CExp = While BExp CExp
          | If BExp CExp CExp
          | Seq CExp CExp
          | Atrib AExp AExp
          | Skip
          | Repeat CExp BExp
          | Do CExp BExp
          | For AExp AExp AExp CExp
          | Swap AExp AExp
          | Dassign AExp AExp AExp AExp
          deriving(Show)



abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = let (n1,s1) = abigStep (e1, s)
                              (n2,s2) = abigStep (e2, s)
                          in (n1+n2,s)
abigStep (Sub e1 e2,s)  = let (n1,s1) = abigStep (e1, s)
                              (n2,s2) = abigStep (e2, s)
                          in (n1-n2,s)
abigStep (Mul e1 e2,s)  = let (n1,s1) = abigStep (e1, s)
                              (n2,s2) = abigStep (e2, s)
                          in (n1*n2,s)


bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s) = (True,s)
bbigStep (FALSE,s) = (False,s)
bbigStep (Not b,s) = case bbigStep (b,s) of
                      (True,_) -> (False, s)
                      (False,_) -> (True, s)

bbigStep (Ig e1 e2,s ) = let (er,s1) = abigStep(e1,s)
                             (er2,s2) = abigStep(e2,s)
                         in  if (er == er2) then (True,s) else (False,s)

bbigStep (And e1 e2,s ) = let(b,s1) = bbigStep(e1,s)
                             (b2,s2) = bbigStep(e2,s)
                         in  if (b && b2) then (True,s) else (False,s)

bbigStep (Or e1 e2,s ) = let(b,s1) = bbigStep(e1,s)
                            (b2,s2) = bbigStep(e2,s)
                         in  if (b || b2) then (True,s) else (False,s)

bbigStep (Leq e1 e2, s) = let (er,s1) = abigStep(e1,s)
                              (er2,s2) = abigStep(e2,s)
                              in if (er <= er2) then (True,s) else (False,s)


cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
  | bbigStep(b,s) == (True,s) = cbigStep(c1,s)
  | otherwise = cbigStep(c2,s)
cbigStep (Seq c1 c2,s)  = let (cr,s2) = cbigStep(c1,s)
                              (cr2,s3) = cbigStep(c2,s2)
                          in (Skip,s3)
cbigStep (Atrib (Var x) e,s) = let (n,s1) = abigStep(e,s)
                                   es = (mudaVar s1 x n)
                               in (Skip, es)
cbigStep (While b c, s)
  | bbigStep(b,s) == (True,s) = cbigStep(Seq c (While b c),s)
  | otherwise = (Skip,s)

cbigStep(Repeat c b, s) = cbigStep(Seq c (If b (Skip) (Repeat c b)),s)
--Repeat C Until B

cbigStep(Do c b, s) = cbigStep(Seq c (If b (Do c b) (Skip) ), s)
--Do C while B

cbigStep(For x e1 e2 c, s) = cbigStep(Seq ( Atrib (Var "x") e1) (If (Leq e1 e2)  (Seq c (For x (Som e1 (Num 1)) e2 c))  (Skip)), s )
-- For x from 1 to 10 do C

cbigStep(Swap (Var x) (Var y), s) =  (Skip, mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))
-- Swap x y

cbigStep(Dassign (Var x) (Var y) e1 e2, s) = let (r,rs) = abigStep(e1,s)
                                                 (r2,rs2) = abigStep(e2,s) 
                                     in cbigStep(Seq (Atrib (Var x) (Num r)) (Atrib (Var y) (Num r2)), s)

meuEstado :: Estado
meuEstado = [("x",250), ("y",1), ("z",2)]

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))


testRepeat = cbigStep( Repeat (Atrib (Var "x") (Som (Var "x") (Num 1))) (Ig (Var "x") (Num 5)), meuEstado )
testDo = cbigStep( Do (Atrib (Var "x") (Som (Var "x") (Num 1))) (Leq (Var "x") (Num 10)), meuEstado )
testSwap = cbigStep( Swap (Var "x") (Var "y"), meuEstado )
testDassign = cbigStep(Dassign (Var "x") (Var "y") (Num 10) (Num 17), meuEstado)
mytest = cbigStep(Seq (Atrib (Var "y") (Num 5)) (Atrib (Var "z") (Num 6)), meuEstado)
testFor = cbigStep( For (Var "x") (Num 5) (Num 20) (Atrib (Var "x") (Som (Var "x") (Num 1))), meuEstado)
fat 0 = 1
fat n = n* fat(n-1)
testFat = cbigStep( fatorial, meuEstado )

ta = Not (Ig(Var"x") (Num 6))
te = Atrib (Var "x") (Som (Var "x") (Num 1))
ts = Atrib (Var "y") (Som (Var "y") (Num 1))
testIf = cbigStep( If ta te ts, meuEstado )
--teste1 :: BExp
--teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
--teste2 :: BExp
--teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


--testec1 :: CExp
--testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
--		(Atrib (Var "y") (Var "z")))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

repcuntilb :: CExp->BExp->Estado -> (CExp,Estado)
repcuntilb ae be o = let (n,s) = cbigStep(ae,o)
                       in cbigStep(If be Skip (While (Not be) ae),s)


exRepeat = repcuntilb (Atrib (Var "x") (Som (Var "x") (Num 1))) ((Ig(Var"x") (Num 6))) (meuEstado)

doCWhileB :: CExp->BExp->Estado -> (CExp,Estado)
doCWhileB ce be o = let (n,s) = cbigStep(ce,o)
                    in cbigStep(If be (While be ce) Skip, s)
					
exDoWhile = doCWhileB (Atrib (Var "x") (Som (Var "x") (Num 1))) (Not (Ig(Var"x") (Num 6))) (meuEstado)

forXdo :: AExp->CExp->CExp->CExp->Estado -> (CExp,Estado)
forXdo (Var x) e1 e2 c o = let (ex1,s1) = cbigStep(e1,o)
                               (ex2,s2) = cbigStep(e2,o)
                               n = procuraVar s2 x
							in cbigStep(While (Not (Ig (Var x) (Num n))) 
							           (Seq c (Atrib (Var x) (Som (Var x) (Num 1)))), s1 )

testForDo = forXdo (Var "x") (Atrib (Var "x") (Num 1)) (Atrib (Var "x") (Num 5)) (Atrib (Var "y") (Som (Var "y") (Num 2))) meuEstado

swap :: (AExp,AExp) -> Estado -> (CExp,Estado)
swap (Var x, Var y) o = let z = procuraVar o x
                            k = procuraVar o y
                            in cbigStep( (Seq (Atrib (Var x) (Var y)) (Atrib (Var y) (Num z))) , o )
                            
                            
testSwp = swap (Var "x", Var "y") meuEstado

dassign :: (AExp,AExp) -> (AExp,AExp) -> Estado -> (CExp,Estado)
dassign (Var x, Var y) (Num e1,Num e2) o = cbigStep( Seq (Atrib (Var x) (Num e1)) (Atrib (Var y) (Num e2)), o )
testDssign = dassign ((Var "x"),(Var "y")) (Num 1, Num 2) meuEstado

--repeat c until b (Done)
--Do c while b (Done)
--For x from E1 to E2 Do c (Done)
-- swap(x,y) (Done)
--x,y := e1,e2  (Done)
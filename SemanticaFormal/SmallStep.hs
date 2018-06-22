data E = Num Int| Soma E E | Mult E E 
	deriving(Eq, Show)

prog1 :: E 
prog1  = Mult (Soma (Num 2) (Num 3)) (Num 4)

smallStepE :: E -> E
smallStepE(Soma (Num n1) (Num n2)) = Num (n1+n2)
smallStepE(Soma(Num n) e) = Soma (Num n)(smallStepE e)
smallStepE (Soma e1 e2) = Soma(smallStepE e1) e2 --case 2
smallStepE(Mult (Num n1)(Num n2)) = Num(n1*n2) ------
smallStepE(Mult(Num n) e) = Mult(Num n)(smallStepE e)
smallStepE(Mult e1 e2) = Mult(smallStepE e1) e2

interpretE :: E -> E
interpretE e = if isFinalE e
			    then e 
			    else interpretE (smallStepE e)

isFinalE :: E -> Bool
isFinalE (Num e) = True
isFinalE _ = False

data B = TRUE | FALSE | Not B | And B B | Or B B 
	deriving(Eq, Show)


smallStepB :: B -> B 
smallStepB(Not TRUE) = FALSE
smallStepB(Not FALSE) = TRUE
smallStepB(Not b) =  Not ( smallStepB (b) ) 
smallStepB(And FALSE b2) = FALSE 
smallStepB(And TRUE b) = b
smallStepB(And b1 b2) = And (smallStepB (b1)) b2

prog2 :: B 
prog2 = And (Not(Not(TRUE))) FALSE


interpretB :: B -> B
interpretB e = if isFinalB e
			    then e 
			    else interpretB (smallStepB e)

isFinalB :: B -> Bool
isFinalB (TRUE) = True
isFinalB (FALSE) = True
isFinalB _ = False



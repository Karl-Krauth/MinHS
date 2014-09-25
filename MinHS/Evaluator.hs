module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import Data.Maybe

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | BinL
           | UnaL Integer
           | BinA (Integer->Integer->Integer)
           | UnaA (Integer->Integer) 
           | BinD (Integer->Integer->Integer)
           | UnaD (Integer->Integer)
           | BinC (Integer->Integer->Bool)
           | UnaC (Integer->Bool)
           | Hea
           | Tai
           | Nul
           | Cl Id [Id] Exp VEnv
           | RecV Bind VEnv

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
evalE g (Var i) 
  | isJust res                     = evalVar (fromJust res)
  | otherwise                      = error ("Undefined variable " ++ i)
  where res                        = E.lookup g i
evalE _ (Prim op)
  | isArith op                     = BinA (arithToFunc op)
  | op == Quot                     = BinD quot
  | op == Rem                      = BinD rem
  | op == Neg                      = UnaA negate
  | isComp op                      = BinC (compToFunc op)
  | op == Head                     = Hea
  | op == Tail                     = Tai
  | op == Null                     = Nul
evalE _ (Con i)
  | i == "True"                    = B True
  | i == "False"                   = B False
  | i == "Nil"                     = Nil
  | i == "Cons"                    = BinL
  | otherwise                      = error "Invalid Con"
evalE _ (Num a)                    = I a
evalE g (App e1 e2)                = evalA (evalE g e1) (evalE g e2)
evalE g (If e1 e2 e3)   
  | getBool c                      = evalE g e2
  | otherwise                      = evalE g e3
  where c                          = evalE g e1
evalE g (Let [] e)                 = evalE g e
evalE g (Let (b:bs) e)             = evalE (evalB g b) (Let bs e)
evalE g (Letfun (Bind i _ [] e))   =  let g' = (E.add g (i,v)) 
                                          v = evalE g' e 
                                      in v
evalE g (Letfun (Bind f _ ids e))  = Cl f ids e env
  where env = E.add g (f, Cl f ids e env)
evalE g (Letrec [] e)              = evalE g e
evalE g (Letrec bs e)              = evalE (evalR g bs) e


evalB :: VEnv -> Bind -> VEnv
evalB g (Bind i _ [] e)  = E.add g (i, evalE g e)
evalB g (Bind i _ ids e) = E.add g (i, Cl "" ids e g)


evalR :: VEnv -> [Bind] -> VEnv
evalR g []                    = g
evalR g (b@(Bind i _ _ _):bs) = evalR (E.add g (i, RecV b (evalR g (b:bs)))) bs


evalVar :: Value -> Value
evalVar (RecV (Bind _ _ [] e) g)  = evalE g e
evalVar (RecV b g)                = evalE g (Letfun b)
evalVar v                         = v


evalA :: Value -> Value -> Value 
evalA BinL (I i)              = UnaL i
evalA (UnaL i) v              = Cons i v
evalA (BinA f) (I i)          = UnaA (f i)
evalA (UnaA f) (I i)          = I (f i)
evalA (BinD f) (I i)          = UnaD (f i)
evalA (UnaD f) (I i) 
  | i == 0                    = error "Division by zero"
  | otherwise                 = I (f i)
evalA (BinC f) (I i)          = UnaC (f i)
evalA (UnaC f) (I i)          = B (f i)
evalA Hea (Cons i _)          = I i 
evalA Hea Nil                 = error "Applying head to empty-list."
evalA Tai (Cons _ v)          = v
evalA Tai Nil                 = error "Applying Tail to empty-list."
evalA Nul (Cons _ _)          = B False
evalA Nul Nil                 = B True
evalA (Cl _ [i] e g) v        = evalE (E.add g (i, v)) e
evalA (Cl f (i:is) e g) v     = Cl f is e (E.add g (i, v))


getBool :: Value -> Bool
getBool (B b) = b
getBool _     = error "Trying to get bool from non boolean value."

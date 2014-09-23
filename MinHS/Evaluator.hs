module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
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

showVal :: Value -> String
showVal (I i)    = "I " ++ (show i)
showVal (B b)      = "B " ++ (show b)
showVal Nil      = "Nil" 
showVal (Cons i v) = "Cons " ++ (show i) ++ " " ++ (showVal v)
showVal BinL     = "BinL" 
showVal (UnaL i)   = "UnaL " ++ show i 
showVal (BinA _)   = "BinA" 
showVal (UnaD _)   = "UnaD" 
showVal (BinC _)     = "BinC" 
showVal (UnaC _)     = "UnaC"
showVal Hea      = "Hea"
showVal Tai      = "Tai"
showVal Nul      = "Nul"
showVal (Cl i is e _) = "Cl " ++ i ++ (show is) ++ (show e)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
--evalE g e = error (show e) --Warning: for testing only
evalE g (Var i) 
  | isJust res                     = fromJust res
  | otherwise                      = error ("Undefined variabe " ++ i)
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
evalE g (Letfun (Bind f _ ids e))  = Cl f ids e g


evalB :: VEnv -> Bind -> VEnv
evalB g (Bind i _ [] e)  = E.add g (i, evalE g e)
evalB g (Bind i _ ids e) = E.add g (i, Cl "" ids e g)


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
evalA c@(Cl f [i] e g) v      = evalE env e
  where env = E.add (E.add g (i, v)) (f, c) 
evalA (Cl f (i:is) e g) v     = Cl f is e (E.add g (i, v))


getBool :: Value -> Bool
getBool (B b) = b
getBool _     = error "Trying to get bool from non boolean value."

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
           | InfA (Integer->Integer->Integer)
           | UnaA (Integer->Integer) 
           | InfC (Integer->Integer->Bool)
           | UnaC (Integer->Bool)
           | Hea ([Integer]->Integer)
           | Tai ([Integer]->[Integer])
           | Nul ([Integer]->Bool)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
evalE g (Var i) 
  | isJust val = fromJust val
  | otherwise = error ("Undefined variabe " ++ i)
  where val = E.lookup g i
evalE g (Prim op)
  | isArith op = InfA (arithToFunc op)
  | op == Neg  = UnaA negate
  | isComp op  = InfC (compToFunc op)
  | op == Head = Hea head
  | op == Tail = Tai tail
  | op == Null = Nul null
evalE _ (Num a) = I a
evalE g (App e1 e2) = evalA (evalE g e1) (evalE g e2)
evalE g (If e1 e2 e3)  
  | getBool c      = v1
  | otherwise      = v2
  where c     = evalE g e1
        v1    = evalE g e2
        v2    = evalE g e3
evalE g (Let [] e) = evalE g e
evalE g (Let (b:bs) e) = evalE (evalB g b) (Let bs e)
evalE g e = error (show e)

evalB :: VEnv -> Bind -> VEnv
evalB g (Bind i t _ e) = E.add g (i, res) 
  where res = evalE g e

evalA :: Value -> Value -> Value
evalA (InfA f) (I i) = UnaA (f i)
evalA (UnaA f) (I i) = I (f i)
evalA (InfC f) (I i) = UnaC (f i)
evalA (UnaC f) (I i) = B (f i)
evalA _ _            = error "Invalid function application."

getBool :: Value -> Bool
getBool (B b) = b
getBool _     = error "Trying to get bool from non boolean value."

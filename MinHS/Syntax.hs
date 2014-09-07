module MinHS.Syntax where

import Data.List

type Id = String

type Program = [Bind]

data Exp
    = Var Id
    | Prim Op
    | Con Id

    | Num Integer

    | App Exp Exp
    | If Exp Exp Exp
    | Let [Bind] Exp
    | Letfun Bind
    | Letrec [Bind] Exp
    deriving (Read,Show,Eq)

data Bind = Bind Id Type [Id] Exp
  deriving (Read,Show,Eq)

data Op = Add
        | Sub
        | Mul
        | Quot
        | Rem
        | Neg
        | Gt
        | Ge
        | Lt
        | Le
        | Eq
        | Ne
        | Head
        | Tail
        | Null
        deriving (Show, Eq, Read)

data Type = Arrow Type Type
          | TypeApp Type Type
          | TypeCon TyCon
          deriving (Read, Show, Eq, Ord)

data TyCon = Unit
           | Bool
           | Int
           | List
           deriving (Read, Show, Eq, Ord)

binApply :: Exp -> Exp -> Exp -> Exp
binApply e1 e2 e3 = App (App e1 e2) e3

binTyApp :: Type -> Type -> Type -> Type
binTyApp t1 t2 t3 = TypeApp (TypeApp t1 t2) t3

isArith :: Op -> Bool
isArith Add  = True
isArith Sub  = True
isArith Mul  = True
isArith _    = False

arithToFunc :: Op -> (Integer -> Integer ->Integer)
arithToFunc Add  = (+)
arithToFunc Sub  = (-)
arithToFunc Mul  = (*)
arithToFunc _    = error "Invalid operator."

isComp :: Op -> Bool
isComp Gt = True
isComp Ge = True
isComp Lt = True
isComp Le = True
isComp Eq = True
isComp Ne = True
isComp _  = False

compToFunc :: Op -> (Integer -> Integer -> Bool)
compToFunc Gt = (>)
compToFunc Ge = (>=)
compToFunc Lt = (<)
compToFunc Le = (<=)
compToFunc Eq = (==)
compToFunc Ne = (/=)
compToFunc _  = error "Invalid operator."

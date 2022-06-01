module Syntax where
import Id
import qualified Type as T

data ExpF f = Unit
            | Bool Bool
            | Int Int
            | Float Double
            | Not (ExpF f)
            | Neg (ExpF f)
            | Add (ExpF f) (ExpF f)
            | Sub (ExpF f) (ExpF f)
            | FNeg (ExpF f)
            | FAdd (ExpF f) (ExpF f)
            | FSub (ExpF f) (ExpF f)
            | FMul (ExpF f) (ExpF f)
            | FDiv (ExpF f) (ExpF f)
            | Eq (ExpF f) (ExpF f)
            | LE (ExpF f) (ExpF f)
            | If (ExpF f) (ExpF f) (ExpF f)
            | Let (Id, T.TypeF f) (ExpF f) (ExpF f)
            | Var Id
            | LetRec (FunDecF f) (ExpF f)
            | App (ExpF f) [ExpF f]
            | Tuple [ExpF f]
            | LetTuple [(Id, T.TypeF f)] (ExpF f) (ExpF f)
            | Array (ExpF f) (ExpF f)
            | Get (ExpF f) (ExpF f)
            | Put (ExpF f) (ExpF f) (ExpF f)
            deriving Show

data FunDecF f = FunDec { name :: (Id, T.TypeF f)
                        , args :: [(Id, T.TypeF f)]
                        , body :: ExpF f
                        }
                 deriving Show

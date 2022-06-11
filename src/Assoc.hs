module Assoc where
import           KNormal (Exp (..), FunDef (..))
import           MyPrelude ()

-- flatten let-bindings (just for prettier printing)
f :: Exp -> Exp
f (IfEq x y e1 e2) = IfEq x y (f e1) (f e2)
f (IfLE x y e1 e2) = IfLE x y (f e1) (f e2)
f (Let xt e1 e2) = let insert (Let yt e3 e4)     = Let yt e3 (insert e4)
                       insert (LetRec fundefs e) = LetRec fundefs (insert e)
                       insert (LetTuple yts z e) = LetTuple yts z (insert e)
                       insert e                  = Let xt e (f e2)
                   in insert (f e1)
f (LetRec (FunDef { name = xt, args = yts, body = e1 }) e2)
  = LetRec (FunDef { name = xt, args = yts, body = f e1 }) (f e2)
f (LetTuple xts y e) = LetTuple xts y (f e)
f e = e

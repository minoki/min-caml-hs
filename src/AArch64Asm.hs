module AArch64Asm where
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Id (Id)
import qualified Id
import           MyPrelude
import qualified Type
import Control.Monad.State.Class

data IdOrImm = V Id
             | C Int
             deriving Show

data Instructions = Ans Exp
                  | Let (Id, Type.Type) Exp Instructions
                  deriving Show

data Exp
  -- 一つ一つの命令に対応する式
  = Nop
  | Mov Id
  | Neg Id
  | Set Int -- mov (16-bit) or mov+movk
  | SetL Id.Label -- adr (±1MB) or adrp+add (±4GB)
  | Add Id IdOrImm -- immediate: 0 <= _ <= 4095
  | Sub Id IdOrImm -- immediate: 0 <= _ <= 4095
  | SLL Id IdOrImm -- shift left logical in SPARC; lsl in AArch64. immediate: 0 <= _ <= 31 (32-bit) or 0 <= _ <= 63 (64-bit)
  | Ld Id IdOrImm -- load word; ldr in AArch64. immediate: 0 <= 4 * _ <= 16380 (32-bit) or 0 <= 8 * _ <= 32760 (64-bit)
  | St Id Id IdOrImm -- store word; str in AArch64. immediate: 0 <= 4 * _ <= 16380 (32-bit) or 0 <= 8 * _ <= 32760 (64-bit)
  | FMovD Id
  | FNegD Id
  | FAddD Id Id
  | FSubD Id Id
  | FMulD Id Id
  | FDivD Id Id
  | LdDF Id IdOrImm -- load double floating-point; ldr in AArch64. immediate: 0 <= 8 * _ <= 32760
  | StDF Id Id IdOrImm -- store double floating-point; str in AArch64. immediate: 0 <= 8 * _ <= 32760
  | Comment String
  -- virtual instructions
  | IfEq Id IdOrImm Instructions Instructions -- cmp immediate: 0 <= _ <= 4095
  | IfLE Id IdOrImm Instructions Instructions -- cmp immediate: 0 <= _ <= 4095
  | IfGE Id IdOrImm Instructions Instructions -- cmp immediate: 0 <= _ <= 4095
  | IfFEq Id Id Instructions Instructions
  | IfFLE Id Id Instructions Instructions
  -- closure address, integer arguments, and float arguments
  | CallCls Id {- integer arguments -} [Id] {- float arguments -} [Id]
  | CallDir Id.Label {- integer arguments -} [Id] {- float arguments -} [Id]
  | Save Id Id
  | Restore Id
  deriving Show

data FunDef = FunDef { name  :: Id.Label
                     , args  :: [Id] -- integer/pointer arguments
                     , fargs :: [Id] -- float arguments
                     , body  :: Instructions
                     , ret   :: Type.Type
                     }
            deriving Show

data Prog = Prog [(Id.Label, Double)] [FunDef] Instructions
          deriving Show

seq :: (MonadState s m, Id.HasCounter s) => Exp -> Instructions -> m Instructions
seq e1 e2 = (\v -> Let (v, Type.Unit) e1 e2) <$> Id.genTmp Type.Unit

-- 先頭の % はemit時に外す
allregs :: [Id]
allregs = [ "%x2", "%x3", "%x4", "%x5"
          , "%x6", "%x7", "%x8", "%x9"
          , "%x10", "%x11", "%x12", "%x13"
          , "%x14", "%x15"
          ]

allfregs :: [Id]
allfregs = ["%d" ++ show (i :: Int) | i <- [0..7] ++ [16..31]]

regs :: V.Vector Id
regs = V.fromList allregs

fregs :: V.Vector Id
fregs = V.fromList allfregs

-- closure address
reg_cl :: Id
reg_cl = allregs !! (length allregs - 2)

-- temporary for swap
reg_sw :: Id
reg_sw = allregs !! (length allregs - 1)

-- temporary for swap
reg_fsw :: Id
reg_fsw = allfregs !! (length allfregs - 1)

-- stack pointer
reg_sp :: Id
reg_sp = "%x0"

-- heap pointer
-- 8-byte aligned
reg_hp :: Id
reg_hp = "%x1"

-- return address
reg_ra :: Id
reg_ra = "%x30"

is_reg :: Id -> Bool
is_reg ('%' : _) = True
is_reg _         = False

removeAndUniq :: Set.Set Id -> [Id] -> [Id]
removeAndUniq xs [] = []
removeAndUniq xs (x : ys) | Set.member x xs = removeAndUniq xs ys
                          | otherwise = x : removeAndUniq (Set.insert x xs) ys

fvIdOrImm :: IdOrImm -> [Id]
fvIdOrImm (V x) = [x]
fvIdOrImm _     = []

-- free variables in the order of use
fvExp :: Exp -> [Id]
fvExp Nop = []
fvExp (Set _) = []
fvExp (SetL _ ) = []
fvExp (Comment _) = []
fvExp (Restore _) = []
fvExp (Mov x) = [x]
fvExp (Neg x) = [x]
fvExp (FMovD x) = [x]
fvExp (FNegD x) = [x]
fvExp (Save x _) = [x]
fvExp (Add x y') = x : fvIdOrImm y'
fvExp (Sub x y') = x : fvIdOrImm y'
fvExp (SLL x y') = x : fvIdOrImm y'
fvExp (Ld x y') = x : fvIdOrImm y'
fvExp (LdDF x y') = x : fvIdOrImm y'
fvExp (St x y y') = x : y : fvIdOrImm y'
fvExp (StDF x y y') = x : y : fvIdOrImm y'
fvExp (FAddD x y) = [x, y]
fvExp (FSubD x y) = [x, y]
fvExp (FMulD x y) = [x, y]
fvExp (FDivD x y) = [x, y]
fvExp (IfEq x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fv e1 ++ fv e2)
fvExp (IfLE x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fv e1 ++ fv e2)
fvExp (IfGE x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fv e1 ++ fv e2)
fvExp (IfFEq x y e1 e2) = x : y : removeAndUniq Set.empty (fv e1 ++ fv e2)
fvExp (IfFLE x y e1 e2) = x : y : removeAndUniq Set.empty (fv e1 ++ fv e2)
fvExp (CallCls x ys zs) = x : ys ++ zs
fvExp (CallDir _ ys zs) = ys ++ zs

fvInstructions :: Instructions -> [Id]
fvInstructions (Ans exp) = fvExp exp
fvInstructions (Let (x, t) exp e) = fvExp exp ++ removeAndUniq (Set.singleton x) (fvInstructions e)

fv :: Instructions -> [Id]
fv e = removeAndUniq Set.empty (fvInstructions e)

concat :: Instructions -> (Id.Id, Type.Type) -> Instructions -> Instructions
concat (Ans exp) xt e2        = Let xt exp e2
concat (Let yt exp e1') xt e2 = Let yt exp (concat e1' xt e2)

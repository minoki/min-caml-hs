module AArch64Asm where
import Prelude hiding (concat)
import qualified Id
import Id (Id)
import qualified Type
import qualified Data.Vector as V

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

data FunDef = FunDef { name :: Id.Label
                     , args :: [Id] -- integer/pointer arguments
                     , fargs :: [Id] -- float arguments
                     , body :: Instructions
                     , ret :: Type.Type
                     }
            deriving Show

data Prog = Prog [(Id.Label, Double)] [FunDef] Instructions
          deriving Show

concat :: Instructions -> (Id.Id, Type.Type) -> Instructions -> Instructions
concat (Ans exp) xt e2 = Let xt exp e2
concat (Let yt exp e1') xt e2 = Let yt exp (concat e1' xt e2)

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

{-# LANGUAGE NumericUnderscores #-}
module Emit where
import           AArch64Asm
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.List as List
import qualified Data.Set as Set
import           GHC.Float (castDoubleToWord64)
import           Id (Id)
import qualified Id
import           Lens.Micro (lens)
import           MyPrelude
import           Numeric
import           System.IO
import           System.Info (os)
import qualified Type

-- 状態：
-- * ラベル生成用カウンター
-- * すでにSaveされた変数の集合 (stackset)
-- * Saveされた変数の、スタックにおける位置 (stackmap)
data S = S !Id.Counter (Set.Set Id) [Id]
type M = ReaderT Handle (StateT S IO)

instance Id.HasCounter S where
  counter = lens (\(S c _ _) -> c) (\(S _ stackset stackmap) c -> S c stackset stackmap)

getStackSet :: M (Set.Set Id)
getStackSet = gets (\(S _ stackset _) -> stackset)

setStackSet :: Set.Set Id -> M ()
setStackSet stackset = modify (\(S counter _ stackmap) -> S counter stackset stackmap)

getStackMap :: M [Id]
getStackMap = gets (\(S _ _ stackmap) -> stackmap)

setStackMap :: [Id] -> M ()
setStackMap stackmap = modify (\(S counter stackset _) -> S counter stackset stackmap)

emit :: String -> M ()
emit s = do h <- ask
            liftIO $ hPutStr h s

save :: Id -> M ()
save x = do S counter stackset stackmap <- get
            let stackset' = Set.insert x stackset
                stackmap' = if x `elem` stackmap then
                              stackmap
                            else
                              stackmap ++ [x]
            put (S counter stackset' stackmap')

savef :: Id -> M ()
savef = save

locate :: Id -> M [Int]
locate x = loc <$> getStackMap
  where loc [] = []
        loc (y : zs) | x == y = 0 : map succ (loc zs)
                     | otherwise = map succ (loc zs)

offset :: Id -> M Int
offset x = do locs <- locate x
              case locs of
                l : _ -> pure (8 * l)
                [] -> do stackmap <- getStackMap
                         fail $ "variable " ++ x ++ " was not found in the stack map " ++ show stackmap

stacksize :: M Int
stacksize = do stackmap <- getStackMap
               pure $ (length stackmap + 1) * 8

reg :: Id -> String
reg ('%' : name) = name
reg name         = "<" ++ name ++ ">" -- for debug

ppIdOrImm :: IdOrImm -> String
ppIdOrImm (V x) = reg x
ppIdOrImm (C i) = '#' : show i

-- 末尾かどうかを表すデータ型
data Dest = Tail Type.Type | NonTail Id

-- 命令列のアセンブリ生成
g :: Dest -> Instructions -> M ()
g dest (Ans exp) = g' dest exp
g dest (Let (x, _) exp e) = do g' (NonTail x) exp
                               g dest e

-- 書く命令のアセンブリ生成
-- 末尾でなかったら計算結果をdestにセット
g' :: Dest -> Exp -> M ()
g' (NonTail _) Nop = pure ()
g' (NonTail x) (Set i)
  | -0x10001 <= i, i <= 0xffff = emit ("\tmov " ++ reg x ++ ", #" ++ show i ++ "\n") -- MOV (wide immediate) or MOV (inverted wide immediate)
  | 0xffff < i, i <= 0xffff_ffff = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .&. 0xffff) ++ "\n")
                                      emit ("\tmovk " ++ reg x ++ ", #" ++ show (i `shiftR` 16) ++ ", lsl #16\n")
  | 0xffff_ffff < i, i <= 0xffff_ffff_ffff = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .&. 0xffff) ++ "\n")
                                                emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 16) .&. 0xffff) ++ ", lsl #16\n")
                                                emit ("\tmovk " ++ reg x ++ ", #" ++ show (i `shiftR` 32) ++ ", lsl #32\n")
  | 0xffff_ffff_ffff < i = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .&. 0xffff) ++ "\n")
                              emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 16) .&. 0xffff) ++ ", lsl #16\n")
                              emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 32) .&. 0xffff) ++ ", lsl #32\n")
                              emit ("\tmovk " ++ reg x ++ ", #" ++ show (i `shiftR` 48) ++ ", lsl #48\n")
  | -0x1_0000_0000 < i = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .|. complement 0x10000) ++ "\n")
                            emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 16) .&. 0xffff) ++ ", lsl #16\n")
  | -0x1_0000_0000_0000 < i = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .|. complement 0x10000) ++ "\n")
                                 emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 16) .&. 0xffff) ++ ", lsl #16\n")
                                 emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 32) .&. 0xffff) ++ ", lsl #32\n")
  | otherwise = do emit ("\tmov " ++ reg x ++ ", #" ++ show (i .|. complement 0x10000) ++ "\n")
                   emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 16) .&. 0xffff) ++ ", lsl #16\n")
                   emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 32) .&. 0xffff) ++ ", lsl #32\n")
                   emit ("\tmovk " ++ reg x ++ ", #" ++ show ((i `shiftR` 48) .&. 0xffff) ++ ", lsl #48\n")
g' (NonTail x) (SetL (Id.Label y)) | os == "darwin" = do emit ("\tadrp " ++ reg x ++ ", " ++ y ++ "@PAGE\n")
                                                         emit ("\tadd " ++ reg x ++ ", " ++ reg x ++ ", " ++ y ++ "@PAGEOFF\n")
                                   | otherwise = do emit ("\tadrp " ++ reg x ++ ", " ++ y ++ "\n")
                                                    emit ("\tadd " ++ reg x ++ ", " ++ reg x ++ ", :lo12:" ++ y ++ "\n")
g' (NonTail x) (Mov y) | x == y = pure ()
                       | otherwise = emit ("\tmov " ++ reg x ++ ", " ++ reg y ++ "\n")
g' (NonTail x) (Neg y) = emit ("\tneg " ++ reg x ++ ", " ++ reg y ++ "\n")
g' (NonTail x) (Add y z') = emit ("\tadd " ++ reg x ++ ", " ++ reg y ++ ", " ++ ppIdOrImm z' ++ "\n")
g' (NonTail x) (Sub y z') = emit ("\tsub " ++ reg x ++ ", " ++ reg y ++ ", " ++ ppIdOrImm z' ++ "\n")
g' (NonTail x) (SLL y z') = emit ("\tlsl " ++ reg x ++ ", " ++ reg y ++ ", " ++ ppIdOrImm z' ++ "\n")
g' (NonTail x) (Ld y z') = emit ("\tldr " ++ reg x ++ ", [" ++ reg y ++ ", " ++ ppIdOrImm z' ++ "]\n") -- LDR (immediate, unsigned offset) or LDR (register)
g' (NonTail _) (St x y z') = emit ("\tstr " ++ reg x ++ ", [" ++ reg y ++ ", " ++ ppIdOrImm z' ++ "]\n") -- STR (immediate, unsigned offset) or STR (register)
g' (NonTail x) (FMovD y) | x == y = pure ()
                         | otherwise = emit ("\tfmov " ++ reg x ++ ", " ++ reg y ++ "\n")
g' (NonTail x) (FNegD y) = emit ("\tfneg " ++ reg x ++ ", " ++ reg y ++ "\n")
g' (NonTail x) (FAddD y z) = emit ("\tfadd " ++ reg x ++ ", " ++ reg y ++ ", " ++ reg z ++ "\n")
g' (NonTail x) (FSubD y z) = emit ("\tfsub " ++ reg x ++ ", " ++ reg y ++ ", " ++ reg z ++ "\n")
g' (NonTail x) (FMulD y z) = emit ("\tfmul " ++ reg x ++ ", " ++ reg y ++ ", " ++ reg z ++ "\n")
g' (NonTail x) (FDivD y z) = emit ("\tfdiv " ++ reg x ++ ", " ++ reg y ++ ", " ++ reg z ++ "\n")
g' (NonTail x) (LdDF y z') = emit ("\tldr " ++ reg x ++ ", [" ++ reg y ++ ", " ++ ppIdOrImm z' ++ "]\n") -- LDR (immediate, unsigned offset) or LDR (register)
g' (NonTail _) (StDF x y z') = emit ("\tstr " ++ reg x ++ ", [" ++ reg y ++ ", " ++ ppIdOrImm z' ++ "]\n") -- STR (immediate, unsigned offset) or STR (register)
g' (NonTail _) (Comment s) = emit ("\t// " ++ s ++ "\n")
-- 退避の仮想命令の実装
g' (NonTail _) (Save x y) = do stackset <- getStackSet
                               if x `elem` allregs && not (Set.member y stackset) then
                                 do save y
                                    o <- offset y
                                    emit ("\tstr " ++ reg x ++ ", [" ++ reg reg_sp ++ ", #" ++ show o ++ "]\n")
                                 else if x `elem` allfregs && not (Set.member y stackset) then
                                 do save y
                                    o <- offset y
                                    emit ("\tstr " ++ reg x ++ ", [" ++ reg reg_sp ++ ", #" ++ show o ++ "]\n")
                                 else if Set.member y stackset then
                                 pure ()
                                 else
                                 error (y ++ " not in stack")
-- 復帰の仮想命令の実装
g' (NonTail x) (Restore y) = do o <- offset y
                                emit ("\tldr " ++ reg x ++ ", [" ++ reg reg_sp ++ ", #" ++ show o ++ "]\n")
-- 末尾だったら計算結果を第一レジスタにセットしてret
g' (Tail _) exp@Nop = do g' (NonTail "_") exp
                         emit "\tret\n"
g' (Tail _) exp@St{} = do g' (NonTail "_") exp
                          emit "\tret\n"
g' (Tail _) exp@StDF{} = do g' (NonTail "_") exp
                            emit "\tret\n"
g' (Tail _) exp@Comment{} = do g' (NonTail "_") exp
                               emit "\tret\n"
g' (Tail _) exp@Save{} = do g' (NonTail "_") exp
                            emit "\tret\n"
g' (Tail _) exp@Set{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@SetL{} = do g' (NonTail (head allregs)) exp
                            emit "\tret\n"
g' (Tail _) exp@Mov{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@Neg{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@Add{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@Sub{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@SLL{} = do g' (NonTail (head allregs)) exp
                           emit "\tret\n"
g' (Tail _) exp@Ld{} = do g' (NonTail (head allregs)) exp
                          emit "\tret\n"
g' (Tail _) exp@FMovD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@FNegD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@FAddD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@FSubD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@FMulD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@FDivD{} = do g' (NonTail (head allfregs)) exp
                             emit "\tret\n"
g' (Tail _) exp@LdDF{} = do g' (NonTail (head allfregs)) exp
                            emit "\tret\n"
g' (Tail t) exp@(Restore x) = do loc <- locate x
                                 case loc of
                                   [_] -> case t of
                                            Type.Float -> g' (NonTail (head allfregs)) exp
                                            _ -> g' (NonTail (head allregs)) exp
                                   _ -> error $ "Restore: invalid location (" ++ show loc ++ ")"
g' (Tail t) (IfEq x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                   g'_tail_if t e1 e2 "b.eq" "b.ne"
g' (Tail t) (IfLE x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                   g'_tail_if t e1 e2 "b.le" "b.gt"
g' (Tail t) (IfGE x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                   g'_tail_if t e1 e2 "b.ge" "b.lt"
g' (Tail t) (IfFEq x y e1 e2) = do emit $ "\tfcmp " ++ reg x ++ ", " ++ reg y ++ "\n"
                                   g'_tail_if t e1 e2 "b.eq" "b.ne"
g' (Tail t) (IfFLE x y e1 e2) = do emit $ "\tfcmp " ++ reg x ++ ", " ++ reg y ++ "\n"
                                   g'_tail_if t e1 e2 "b.ls" "b.hi" -- less than or equal / greater than or unordered
g' (NonTail z) (IfEq x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                      g'_non_tail_if (NonTail z) e1 e2 "b.eq" "b.ne"
g' (NonTail z) (IfLE x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                      g'_non_tail_if (NonTail z) e1 e2 "b.le" "b.gt"
g' (NonTail z) (IfGE x y' e1 e2) = do emit $ "\tcmp " ++ reg x ++ ", " ++ ppIdOrImm y' ++ "\n"
                                      g'_non_tail_if (NonTail z) e1 e2 "b.ge" "b.lt"
g' (NonTail z) (IfFEq x y e1 e2) = do emit $ "\tfcmp " ++ reg x ++ ", " ++ reg y ++ "\n"
                                      g'_non_tail_if (NonTail z) e1 e2 "b.eq" "b.ne"
g' (NonTail z) (IfFLE x y e1 e2) = do emit $ "\tfcmp " ++ reg x ++ ", " ++ reg y ++ "\n"
                                      g'_non_tail_if (NonTail z) e1 e2 "b.ls" "b.hi" -- less than or equal / greater than or unordered
-- 関数呼び出しの仮想命令の実装
g' (Tail _) (CallCls x ys zs) = do g'_args [(x, reg_cl)] ys zs
                                   emit $ "\tldr " ++ reg reg_sw ++ ", [" ++ reg reg_cl ++ "]\n"
                                   emit $ "\tbr " ++ reg reg_sw ++ "\n"
g' (Tail _) (CallDir (Id.Label x) ys zs) = do g'_args [] ys zs
                                              emit $ "\tb " ++ x ++ "\n"
g' (NonTail a) (CallCls x ys zs) = do
  g'_args [(x, reg_cl)] ys zs
  ss <- stacksize
  emit $ "\tstr " ++ reg reg_ra ++ ", [" ++ reg reg_sp ++ ", #" ++ show (ss - 8) ++ "]\n"
  emit $ "\tldr " ++ reg reg_sw ++ ", [" ++ reg reg_cl ++ "]\n"
  emit $ "\tadd " ++ reg reg_sp ++ ", " ++ reg reg_sp ++ ", #" ++ show ss ++ "\n"
  emit $ "\tblr " ++ reg reg_sw ++ "\n"
  emit $ "\tsub " ++ reg reg_sp ++ ", " ++ reg reg_sp ++ ", #" ++ show ss ++ "\n"
  emit $ "\tldr " ++ reg reg_ra ++ ", [" ++ reg reg_sp ++ ", #" ++ show (ss - 8) ++ "]\n"
  if a `elem` allregs && a /= head allregs then
    emit $ "\tmov " ++ reg (head allregs) ++ ", " ++ reg a ++ "\n"
    else if a `elem` allfregs && a /= head allfregs then
    emit $ "\tfmov " ++ reg (head allfregs) ++ ", " ++ reg a ++ "\n"
    else
    pure ()
g' (NonTail a) (CallDir (Id.Label x) ys zs) = do
  g'_args [] ys zs
  ss <- stacksize
  emit $ "\tstr " ++ reg reg_ra ++ ", [" ++ reg reg_sp ++ ", #" ++ show (ss - 8) ++ "]\n"
  emit $ "\tadd " ++ reg reg_sp ++ ", " ++ reg reg_sp ++ ", #" ++ show ss ++ "\n"
  emit $ "\tbl " ++ x ++ "\n"
  emit $ "\tsub " ++ reg reg_sp ++ ", " ++ reg reg_sp ++ ", #" ++ show ss ++ "\n"
  emit $ "\tldr " ++ reg reg_ra ++ ", [" ++ reg reg_sp ++ ", #" ++ show (ss - 8) ++ "]\n"
  if a `elem` allregs && a /= head allregs then
    emit $ "\tmov " ++ reg (head allregs) ++ ", " ++ reg a ++ "\n"
    else if a `elem` allfregs && a /= head allfregs then
    emit $ "\tfmov " ++ reg (head allfregs) ++ ", " ++ reg a ++ "\n"
    else
    pure ()

g'_tail_if :: Type.Type -> Instructions -> Instructions -> String -> String -> M ()
g'_tail_if t e1 e2 b bn = do
  b_else <- Id.genId (b ++ "_else")
  emit $ "\t" ++ bn ++ " " ++ b_else ++ "\n"
  stackset_back <- getStackSet
  g (Tail t) e1
  emit $ b_else ++ ":\n"
  setStackSet stackset_back
  g (Tail t) e2

g'_non_tail_if :: Dest -> Instructions -> Instructions -> String -> String -> M ()
g'_non_tail_if dest e1 e2 b bn = do
  b_else <- Id.genId (b ++ "_else")
  b_cont <- Id.genId (b ++ "_cont")
  emit $ "\t" ++ bn ++ " " ++ b_else ++ "\n"
  stackset_back <- getStackSet
  g dest e1
  stackset1 <- getStackSet
  emit $ "\tb " ++ b_cont ++ "\n"
  emit $ b_else ++ ":\n"
  setStackSet stackset_back
  g dest e2
  stackset2 <- getStackSet
  emit $ b_cont ++ ":\n"
  setStackSet $ Set.intersection stackset1 stackset2

-- >>> shuffle "tmp" [("a","b"),("b","c"),("c","a"),("d","e"),("e","d")]
-- [("b","tmp"),("a","b"),("c","a"),("tmp","c"),("e","tmp"),("d","e"),("tmp","d")]
shuffle :: Eq a => a -> [(a, a)] -> [(a, a)]
shuffle sw xys = let xys' = filter (\(x, y) -> x /= y) xys -- remove identical moves
                 in case List.partition (\(_, y) -> List.any (\(x, _) -> x == y) xys') xys' of -- find acyclic moves
                      ([], []) -> []
                      ((x, y) : xys, []) -> -- no acyclic moves; resolve a cyclic move
                        (y, sw) : (x, y) : shuffle sw (map (\yz@(y', z) -> if y == y' then (sw, z) else yz) xys)
                      (xys, acyc) -> acyc ++ shuffle sw xys

g'_args :: [(Id, Id)] -> [Id] -> [Id] -> M ()
g'_args x_reg_cl ys zs = do
  let yrs = List.foldl' (\yrs (y, r) -> (y, r) : yrs) x_reg_cl (zip ys allregs)
  forM_ (shuffle reg_sw yrs) $ \(y, r) ->
    emit $ "\tmov " ++ reg r ++ ", " ++ reg y ++ "\n"
  let zfrs = List.foldl' (\zfrs (z, fr) -> (z, fr) : zfrs) [] (zip zs allfregs)
  forM_ (shuffle reg_fsw zfrs) $ \(z, fr) ->
    emit $ "\tfmov " ++ reg fr  ++ ", " ++ reg z ++ "\n"

h :: FunDef -> M ()
h (FunDef { name = Id.Label x, args = _, fargs = _, body = e, ret = t }) = do
  emit $ x ++ ":\n"
  setStackSet Set.empty
  setStackMap []
  g (Tail t) e

runM :: M a -> Handle -> StateT Id.Counter IO a
runM m h = StateT $ \counter -> do
  (result, S counter' _ _) <- runStateT (runReaderT m h) (S counter Set.empty [])
  pure (result, counter')

f :: Handle -> Prog -> StateT Id.Counter IO ()
f oc (Prog dat fundefs e) = do
  lift $ do
    hPutStrLn stderr "generating assembly..."
    hPutStr oc "\t.text\n"
    hPutStr oc "\t.align 3\n" -- 8-byte aligned
    forM_ dat $ \(Id.Label x, d) -> do
      hPutStr oc $ x ++ ": // " ++ show d ++ "\n"
      let h = castDoubleToWord64 d
          hi = h `shiftR` 32
          lo = h .&. 0xffff_ffff
      -- assume little endian
      hPutStr oc $ "\t.long 0x" ++ showHex lo "\n"
      hPutStr oc $ "\t.long 0x" ++ showHex hi "\n"
    hPutStr oc "\t.text\n"
  forM_ fundefs $ \fundef -> runM (h fundef) oc
  lift $ do
    -- for Linux:
    hPutStr oc "\t.global min_caml_start\n"
    hPutStr oc "min_caml_start:\n"
    -- for macOS:
    hPutStr oc "\t.global _min_caml_start\n"
    hPutStr oc "_min_caml_start:\n"
    -- Prologue
    hPutStr oc "\tstp x29, x30, [sp, #-16]!\n"
    hPutStr oc "\tstp x27, x28, [sp, #-16]!\n"
    hPutStr oc "\tadd x29, sp, #32\n"
    hPutStr oc "\tmov x27, x0\n"
    hPutStr oc "\tmov x28, x1\n"
  runM (g (NonTail "%x0") e) oc -- destination register?
  lift $ do
    -- Epilogue
    hPutStr oc "\tldp x27, x28, [sp], #16\n"
    hPutStr oc "\tldp x29, x30, [sp], #16\n"
    hPutStr oc "\tret\n"

module Main where
import qualified AArch64.Emit
import qualified AArch64.RegAlloc
import qualified AArch64.Simm
import qualified AArch64.Virtual
import qualified Alpha
import qualified Assoc
import qualified Beta
import qualified Closure
import qualified ConstFold
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import qualified Elim
import           GHC.Foreign (peekCStringLen)
import qualified Id
import qualified Inline
import qualified KNormal
import qualified Lexer
import           MyPrelude
import qualified Options.Applicative as OA
import qualified Parser
import           System.Exit
import           System.IO
import qualified Typing

data Options = Options { inline             :: !Int
                       , iterLimit          :: !Int
                       , printIntermediates :: !Bool
                       , filename           :: String
                       }

options :: OA.Parser Options
options = Options <$> OA.option OA.auto (OA.long "inline" <> OA.help "Maximum size of functions inlined" <> OA.value 0 <> OA.metavar "m")
          <*> OA.option OA.auto (OA.long "iter" <> OA.help "Maximum number of optimizations iterated" <> OA.value 1000 <> OA.metavar "n")
          <*> OA.switch (OA.long "print-intermediates" <> OA.help "Print intermediate representations")
          <*> OA.argument OA.str (OA.metavar "NAME")

iter :: Int -> Int -> KNormal.Exp -> StateT Id.Counter IO KNormal.Exp
iter threshold n e = do
  lift $ hPutStrLn stderr $ "iteration " ++ show n
  if n == 0 then
    pure e
    else
    do e' <- lift $ Assoc.f <$> Beta.f e
       e'' <- Inline.f e' threshold
       e''' <- Elim.f (ConstFold.f e'')
       if e == e''' then
         pure e
         else
         iter threshold (n - 1) e'''

main :: IO ()
main = do
  options <- OA.execParser $ OA.info (options OA.<**> OA.helper) (OA.fullDesc <> OA.header "Min-Caml Compiler for AArch64")
  let inputFilename = filename options ++ ".ml"
      outputFilename = filename options ++ ".s"
  bs <- BS.readFile inputFilename
  s <- BS.useAsCStringLen bs $ \cs ->
    peekCStringLen utf8 cs <|> do hPutStrLn stderr "Input file is not UTF-8 encoded; trying EUC-JP..."
                                  euc_jp <- mkTextEncoding "euc-jp"
                                  peekCStringLen euc_jp cs
  let compile :: StateT Id.Counter (ExceptT String IO) ()
      compile = do tokens <- Lexer.runLexer s
                   exp <- StateT $ ExceptT . pure . runStateT (Parser.parseExp tokens)
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Parse ==="
                     print exp
                     putStrLn "============="
                   (exp', extenv) <- lift $ ExceptT (Typing.f exp)
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Type Check ==="
                     print exp'
                     putStrLn "=================="
                   exp'' <- KNormal.f exp' extenv
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== KNormal ==="
                     print exp''
                     putStrLn "==============="
                   exp''' <- Alpha.f exp''
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Alpha ==="
                     print exp'''
                     putStrLn "============="
                   exp'''' <- StateT $ lift . runStateT (iter (inline options) (iterLimit options) exp''')
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Optimized ==="
                     print exp''''
                     putStrLn "================="
                   prog <- Closure.f exp''''
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Closure ==="
                     print prog
                     putStrLn "==============="
                   prog' <- AArch64.Virtual.f prog
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Virtual ==="
                     print prog'
                     putStrLn "==============="
                   prog'' <- AArch64.Simm.f prog'
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== Simm ==="
                     print prog'
                     putStrLn "============"
                   prog''' <- AArch64.RegAlloc.f prog''
                   liftIO $ when (printIntermediates options) $ do
                     putStrLn "=== RegAlloc ==="
                     print prog''
                     putStrLn "================"
                   StateT $ \state -> lift $ withFile outputFilename WriteMode $ \out -> do
                     runStateT (AArch64.Emit.f out prog''') state
  result <- runExceptT (runStateT compile Id.initialCounter)
  case result of
    Left e -> do hPutStrLn stderr e
                 exitFailure
    Right ((), _state) -> pure ()

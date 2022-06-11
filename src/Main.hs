module Main where
import qualified AArch64.Emit
import qualified AArch64.RegAlloc
import qualified AArch64.Virtual
import qualified Alpha
import qualified Assoc
import qualified Beta
import qualified Closure
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Data.Functor.Identity
import           GHC.Foreign (peekCStringLen)
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

iter :: Int -> KNormal.Exp -> IO KNormal.Exp
iter n e = do hPutStrLn stderr $ "iteration " ++ show n
              if n == 0 then
                pure e
              else
                do let e' = Assoc.f $ runIdentity (Beta.f e)
                   if e == e' then
                     pure e
                   else
                     iter (n - 1) e'

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
  case Lexer.runAlex s Lexer.scanAllAndState of
    Left msg -> do hPutStrLn stderr ("Lexical error: " ++ msg)
                   exitFailure
    Right (tokens, state) ->
      case runStateT (Parser.parseExp tokens) state of
        Left msg -> do hPutStrLn stderr msg
                       exitFailure
        Right (exp, state') -> do
          when (printIntermediates options) $ do
            putStrLn "=== Parse ==="
            print exp
            putStrLn "============="
          case Typing.f exp of
            Left msg -> do hPutStrLn stderr msg
                           exitFailure
            Right (exp', extenv) -> do
              when (printIntermediates options) $ do
                putStrLn "=== Type Check ==="
                print exp'
                putStrLn "=================="
              case runStateT (runReaderT (KNormal.f exp') extenv) state' of
                Left msg -> do hPutStrLn stderr msg
                               exitFailure
                Right (exp'', state'') -> do
                  when (printIntermediates options) $ do
                    putStrLn "=== KNormal ==="
                    print exp''
                    putStrLn "==============="
                  case runState (Alpha.f exp'') state'' of
                    (exp''', state''') -> do
                      when (printIntermediates options) $ do
                        putStrLn "=== Alpha ==="
                        print exp'''
                        putStrLn "============="
                      exp'''' <- iter (iterLimit options) exp'''
                      case Closure.f exp'''' of
                        prog@(Closure.Prog _ _) -> do
                          when (printIntermediates options) $ do
                            putStrLn "=== Closure ==="
                            print prog
                            putStrLn "==============="
                          case AArch64.Virtual.f prog state''' of
                            Left msg -> do hPutStrLn stderr msg
                                           exitFailure
                            Right (prog', state'''') -> do
                              when (printIntermediates options) $ do
                                putStrLn "=== Virtual ==="
                                print prog'
                                putStrLn "==============="
                              case runStateT (AArch64.RegAlloc.f prog') state'''' of
                                Left msg -> do hPutStrLn stderr msg
                                               exitFailure
                                Right (prog'', state''''') -> do
                                  when (printIntermediates options) $ do
                                    putStrLn "=== RegAlloc ==="
                                    print prog''
                                    putStrLn "================"
                                  withFile outputFilename WriteMode $ \out -> do
                                    ((), _) <- runStateT (AArch64.Emit.f out prog'') state'''''
                                    pure ()

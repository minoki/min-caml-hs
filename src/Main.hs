module Main where
import qualified Alpha
import qualified Closure
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Emit
import           GHC.Foreign (peekCStringLen)
import           GHC.IO.Encoding (mkTextEncoding, utf8)
import qualified KNormal
import qualified Lexer
import           MyPrelude
import qualified Options.Applicative as OA
import qualified Parser
import qualified RegAlloc
import           System.Exit
import           System.IO
import qualified Typing
import qualified Virtual

data Options = Options { inline             :: !Int
                       , iter               :: !Int
                       , printIntermediates :: !Bool
                       , filename           :: String
                       }

options :: OA.Parser Options
options = Options <$> OA.option OA.auto (OA.long "inline" <> OA.help "Maximum size of functions inlined" <> OA.value 0 <> OA.metavar "m")
          <*> OA.option OA.auto (OA.long "iter" <> OA.help "Maximum number of optimizations iterated" <> OA.value 1000 <> OA.metavar "n")
          <*> OA.switch (OA.long "print-intermediates" <> OA.help "Print intermediate representations")
          <*> OA.argument OA.str (OA.metavar "NAME")

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
                      case Closure.f exp''' of
                        prog@(Closure.Prog _ _) -> do
                          when (printIntermediates options) $ do
                            putStrLn "=== Closure ==="
                            print prog
                            putStrLn "==============="
                          case Virtual.f prog state''' of
                            Left msg -> do hPutStrLn stderr msg
                                           exitFailure
                            Right (prog', state'''') -> do
                              when (printIntermediates options) $ do
                                putStrLn "=== Virtual ==="
                                print prog'
                                putStrLn "==============="
                              case runStateT (RegAlloc.f prog') state'''' of
                                Left msg -> do hPutStrLn stderr msg
                                               exitFailure
                                Right (prog'', state''''') -> do
                                  when (printIntermediates options) $ do
                                    putStrLn "=== RegAlloc ==="
                                    print prog''
                                    putStrLn "================"
                                  withFile outputFilename WriteMode $ \out -> do
                                    ((), _) <- runStateT (Emit.f out prog'') state'''''
                                    pure ()

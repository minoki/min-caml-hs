module Main where
import qualified Alpha
import qualified Closure
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Emit
import qualified KNormal
import qualified Lexer
import           MyPrelude
import qualified Options.Applicative as OA
import qualified Parser
import qualified RegAlloc
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
  s <- readFile inputFilename
  case Lexer.runAlex s Lexer.scanAllAndState of
    Left msg -> hPutStrLn stderr ("Lexical error: " ++ msg)
    Right (tokens, state) ->
      case runStateT (Parser.parseExp tokens) state of
        Left msg -> hPutStrLn stderr msg
        Right (exp, state') -> do
          when (printIntermediates options) $ do
            putStrLn "=== Parse ==="
            print exp
            putStrLn "============="
          case Typing.f exp of
            Left msg -> hPutStrLn stderr msg
            Right (exp', extenv) -> do
              when (printIntermediates options) $ do
                putStrLn "=== Type Check ==="
                print exp'
                putStrLn "=================="
              case runStateT (runReaderT (KNormal.f exp') extenv) state' of
                Left msg -> hPutStrLn stderr msg
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
                            Left msg -> hPutStrLn stderr msg
                            Right (prog', state'''') -> do
                              when (printIntermediates options) $ do
                                putStrLn "=== Virtual ==="
                                print prog'
                                putStrLn "==============="
                              let (prog'', state''''') = runState (RegAlloc.f prog') state''''
                              when (printIntermediates options) $ do
                                putStrLn "=== RegAlloc ==="
                                print prog''
                                putStrLn "================"
                              withFile outputFilename WriteMode $ \out -> do
                                ((), _) <- runStateT (Emit.f out prog'') state'''''
                                pure ()

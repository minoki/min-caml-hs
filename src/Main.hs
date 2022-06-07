module Main where
import qualified Lexer
import qualified Parser
import qualified Typing
import qualified KNormal
import qualified Alpha
import qualified Closure
import qualified Virtual
import qualified RegAlloc
import qualified Emit
import Control.Monad.State.Strict
import Control.Monad.Reader
import System.IO

main :: IO ()
main = do
  s <- getContents
  case Lexer.runAlex s Lexer.scanAllAndState of
    Left msg -> putStrLn ("Lexical error: " ++ msg)
    Right (tokens, state) ->
      case runStateT (Parser.parseExp tokens) (Lexer.idCounter state) of
        Left msg -> putStrLn msg
        Right (exp, state') ->
          case Typing.f exp of
            Left msg -> putStrLn msg
            Right (exp', extenv) -> case runStateT (runReaderT (KNormal.f exp') extenv) state' of
              Left msg -> putStrLn msg
              Right (exp'', state'') -> case runState (Alpha.f exp'') state'' of
                (exp''', state''') -> do
                  print exp'''
                  case Closure.f exp''' of
                    prog@(Closure.Prog _ _) -> do
                      case Virtual.f prog state''' of
                        Left msg -> putStrLn msg
                        Right (prog', state'''') -> do
                          let prog'' = RegAlloc.f prog'
                          print prog''
                          ((), _) <- runStateT (Emit.f stdout prog'') state''''
                          pure ()

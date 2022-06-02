module Main where
import qualified Lexer
import qualified Parser
import Control.Monad.State.Strict

main :: IO ()
main = do
  s <- getContents
  case Lexer.runAlex s Lexer.scanAllAndState of
    Left msg -> putStrLn ("Lexical error: " ++ msg)
    Right (tokens, state) -> case runStateT (Parser.parseExp tokens) (Lexer.idCounter state) of
                               Left msg -> putStrLn msg
                               Right exp -> print exp

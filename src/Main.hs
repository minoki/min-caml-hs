module Main where
import qualified Lexer
import qualified Parser

main :: IO ()
main = do
  s <- getContents
  case Lexer.runAlex s Lexer.scanAll of
    Left msg -> putStrLn ("Lexical error: " ++ msg)
    Right tokens -> print (Parser.parseExp tokens)

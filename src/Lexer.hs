module Lexer where

import Control.Monad.State
import Data.Char

type Lexeme = String
type Lexer l = State String l

runLexer :: Lexer l -> String -> (l, String)
runLexer = runState

lexResult :: Lexer l -> String -> l
lexResult = evalState

lexWord :: Lexer Lexeme
lexWord = state $ break isSpace . dropWhile isSpace

lexSkipChar :: Lexer ()
lexSkipChar = modify $ \s -> if null s then s else tail s

lexUntilSep :: Char -> Lexer Lexeme
lexUntilSep sep = state $ break (sep==)

lexBracketed :: Char -> Char -> Lexer Lexeme
lexBracketed  open close = do
  lexUntilSep open
  lexSkipChar
  b <- lexUntilSep close
  lexSkipChar
  return b

----

-- | accessLog for sample parser

accessLog :: String -> [String]
accessLog = lexResult $ sequence [lex_h, lex_l, lex_u, lex_t, lex_r, lex_s, lex_b]
  where lex_h = lexWord
        lex_l = lexWord
        lex_u = lexWord
        lex_t = lexBracketed '[' ']'
        lex_r = lexBracketed '"' '"'
        lex_s = lexWord
        lex_b = lexWord

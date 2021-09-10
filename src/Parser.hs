module Parser where

import Data.Char
import Control.Monad
import Control.Monad.State

type Parser t a = StateT [t] [] a

runParser :: Parser t a -> [t] -> [(a, [t])]
runParser = runStateT

failure :: Parser t a
failure = mzero

succeed :: a -> Parser t a
succeed = return

item :: Parser t t
item = do { (x:xs) <- get
          ; put xs
          ; return x
          }

sat :: (t -> Bool) -> Parser t t
sat p = do { x <- item
           ; if p x then return x else failure
           }
  
alt :: Parser t a -> Parser t a -> Parser t a
alt = mplus

many :: Parser t a -> Parser t [a]
many p = many1 p `alt` return []

many1 :: Parser t a -> Parser t [a]
many1 p = do { x <- p
             ; xs <- many p
             ; return (x:xs)
             }

----
greeting :: Parser String (String, String)
greeting = do { hg <- helloOrGoodbye
              ; x <- item
              ; return (hg, x)
              }

helloOrGoodbye :: Parser String String
helloOrGoodbye = hello `alt` goodbye

hello :: Parser String String
hello = sat ("Hello"==)

goodbye :: Parser String String
goodbye = sat ("Goodbye"==)

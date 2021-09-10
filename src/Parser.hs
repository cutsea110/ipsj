module Parser where

import Data.Char
import Control.Monad
import Control.Monad.State

import Ticket

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

-----

pterm :: Parser Char Term
pterm = papp `alt` pval

papp :: Parser Char Term
papp = do { sat ('('==)
          ; l <- pterm
          ; o <- pbop
          ; r <- pterm
          ; sat (')'==)
          ; return (App o l r)
          }

pval :: Parser Char Term
pval = do { c <- sat (`elem` ['0'..'9'])
          ; return (Val c)
          }

pbop :: Parser Char Char
pbop = sat (`elem` "+-*/")

instance Read Term where
  readsPrec _ = runParser pterm

----

char :: Char -> Parser Char Char
char c = sat (c==)

pS :: Parser Char String
pS = do { a  <- pa
        ; bc <- pB
        ; d  <- pd
        ; return ([a]++bc++[d])
        }

pB :: Parser Char String
pB = (do { b <- pb; return [b] }) `alt`
     (do { b <- pb; c <- pc; return (b:[c])})

pa :: Parser Char Char
pa = char 'a'
pb :: Parser Char Char
pb = char 'b'
pc :: Parser Char Char
pc = char 'c'
pd :: Parser Char Char
pd = char 'd'

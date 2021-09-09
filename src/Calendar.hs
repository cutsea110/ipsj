module Calendar where

import Zeller

intToString :: Int -> String
intToString n = [" 123"!!a, ['0'..'9']!!b]
  where (a, b) = n `divMod` 10

intsToString :: [Int] -> Int -> String
intsToString ns ld = concatMap d ns
  where d x | x <= 0 || x > ld = "   "
            | otherwise = intToString x ++ " "

monthnames :: [String]
monthnames = [ "January", "February", "March"
             , "April"  , "May"     , "June"
             , "July"   , "August"  , "September"
             , "October", "November", "December"
             ]

month :: Int -> String
month m = expand (monthnames !! (m-1))

expand :: String -> String
expand s = let leng = length s
               padlen = (20 - leng) `div` 2
           in take 21 (replicate padlen ' ' ++ s ++ repeat ' ')

cal :: Int -> Int -> IO ()
cal y m = do
  putStrLn head
  putStrLn (unlines (cc y m))
    where head = expand ((monthnames !! (m-1)) ++ " " ++ year)
          year = show y

daynames :: String
daynames = " S  M Tu  W Th  F  S "

leap :: Int -> Int
leap y = dif 4 - dif 100 + dif 400
  where dif d = y `div` d - y1 `div` d
        y1 = y-1

cc :: Int -> Int -> [String]
cc y m = let z = 1 - zeller y m 1
             ld = [31, 28+leap y, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m-1)
         in daynames : map (`intsToString` ld) [[d..d+6]|d<-[z,z+7..z+35]]

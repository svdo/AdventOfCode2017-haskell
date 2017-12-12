module Day9
  ( stream
  ) where

-- Stream -> Group
-- Group -> '{' Things '}'
-- Things -> Thing | Thing ',' Things
-- Things -> Group | Garbage
-- Garbage -> '<' AnyCharExceptGtEscaped* '>'
-- AnythingExceptGtEscaped -> '!' Char | AnythingExceptGt
-- AnythingExceptGt -> Char \\ '>'

stream :: String -> (String, Bool, Int)
stream xs
  | not valid       = (remaining, False, count)
  | null remaining  = ([], True, count)
  | otherwise       = (remaining, False, count)
  where (remaining, valid, count) = group xs

group :: String -> (String, Bool, Int)
group ('{':xs)
  | null xs                 = ([], False, count)
  | head afterThings == '}' = (tail afterThings, True, count+1)
  | otherwise               = (afterThings, False, count)
  where (afterThings, valid, count) = things xs

things :: String -> (String, Bool, Int)
things [] = ([], True, 0)
things ('}':xs) = ('}':xs, True, 0)
things ('{':xs) = group ('{':xs)



--  | not valid               = (afterThings, False)
--  | null xs                 = ([], False)
--  | head afterThings == '}' = (tail afterThings, True)
--  | otherwise               = (afterThings, False)
--  where
--    (afterThings, valid) = things xs
--
--things :: String -> (String, Bool)
--things = thing
--
--thing :: String -> (String, Bool)
--thing [] = ([], True)
--thing xs = garbage xs
--
--garbage :: String -> (String, Bool)
--garbage [] = ([], False)
--garbage ('<':xs)
--  | not valid              = (xs, False)
--  | null xs                = ([], False)
--  | head afterChars == '>' = (tail afterChars, True)
--  | otherwise              = (afterChars, False)
--  where (afterChars, valid) = manyCharsExceptGt xs
--
--manyCharsExceptGt :: String -> (String, Bool)
--manyCharsExceptGt (x:xs)
--  | x == '>'  = (x:xs, True)
--  | otherwise = manyCharsExceptGt xs

--import qualified Text.Parsec as Parsec
-- I am the error message infix operator, used later:
--import Text.Parsec ((<?>))
--import Control.Applicative

-- alias Parsec.parse for more concise usage in my examples:
--parse rule text = Parsec.parse rule "(source)" text

--anythingExceptGt :: Parsec.Parsec String () ()
--anythingExceptGt = Parsec.noneOf ['>']

--escape :: Parsec.Parsec String () ()
--escape = Parsec.char '!' >> Parsec.anyChar

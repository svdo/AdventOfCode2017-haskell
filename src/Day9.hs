module Day9
  ( stream
    , garbage
  ) where

--import Debug.Trace

-- Stream -> Group
-- Group -> '{' Things '}'
-- Things -> Thing ',' Things | Thing
-- Thing -> Group | Garbage
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
--group xs | trace ("group " ++ xs) False = undefined
group ('{':xs)
  | null xs                 = ([], False, count)
  | head endOfGroup == '}'  = (tail endOfGroup, True, count+1)
  | otherwise               = (endOfGroup, False, count)
  where (endOfGroup, valid, count) = things xs
group xs = (xs, False, 0)

things :: String -> (String, Bool, Int)
--things xs | trace ("things " ++ xs) False = undefined
things [] = ([], True, 0)
things xs
  | head endOfThing == ',' = (endOfMoreThings, moreValid, count+moreCount)
  | otherwise              = (endOfThing, valid, count)
  where (endOfThing, valid, count) = thing xs
        (endOfMoreThings, moreValid, moreCount) = things (tail endOfThing)

thing :: String -> (String, Bool, Int)
--thing xs | trace ("thing " ++ xs) False = undefined
thing ('}':xs) = ('}':xs, True, 0)
thing xs
  | validGroup = (remaining, validGroup, count)
  | otherwise  = garbage xs
  where (remaining, validGroup, count) = group xs

garbage :: String -> (String, Bool, Int)
garbage ('<':xs)
  | head endOfGarbage == '>' = (tail endOfGarbage, True, count)
  where (endOfGarbage, valid, count) = anythingExceptGt xs

anythingExceptGt :: String -> (String, Bool, Int)
anythingExceptGt ('!':x:xs) = anythingExceptGt xs
anythingExceptGt ('>':xs) = ('>':xs, True, 0)
anythingExceptGt (x:xs) = anythingExceptGt xs


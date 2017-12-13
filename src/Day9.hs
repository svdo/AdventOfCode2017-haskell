module Day9
  ( score
  , stream
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

type Count = Int
type IsValid = Bool
type NestingLevel = Int
type ParseResult = (String, IsValid, Count, [NestingLevel])
type ParseRule = String -> NestingLevel -> ParseResult

score :: String -> Int
score xs = sum levels
  where (_,_,_,levels) = stream xs

stream :: String -> ParseResult
stream xs
  | not valid       = (remaining, False, count, levels)
  | null remaining  = ([], True, count, levels)
  | otherwise       = (remaining, False, count, levels)
  where (remaining, valid, count, levels) = group xs 1

group :: ParseRule
--group xs | trace ("group " ++ xs) False = undefined
group ('{':xs) nesting
  | null xs                 = ([], False, count, levels)
  | head endOfGroup == '}'  = (tail endOfGroup, True, count+1, nesting:levels)
  | otherwise               = (endOfGroup, False, count, levels)
  where (endOfGroup, valid, count, levels) = things xs nesting
group xs _ = (xs, False, 0, [])

things :: ParseRule
--things xs | trace ("things " ++ xs) False = undefined
things [] nesting = ([], True, 0, [])
things xs nesting
  | head endOfThing == ',' = (endOfMoreThings, moreValid, count+moreCount, levels ++  moreLevels)
  | otherwise              = (endOfThing, valid, count, levels)
  where (endOfThing, valid, count, levels) = thing xs nesting
        (endOfMoreThings, moreValid, moreCount, moreLevels) = things (tail endOfThing) nesting

thing :: ParseRule
--thing xs | trace ("thing " ++ xs) False = undefined
thing ('}':xs) nesting = ('}':xs, True, 0, [])
thing xs nesting
  | validGroup = (remaining, validGroup, count, levels)
  | otherwise  = garbage xs nesting
  where (remaining, validGroup, count, levels) = group xs (nesting+1)

garbage :: ParseRule
garbage ('<':xs) nesting
  | head endOfGarbage == '>' = (tail endOfGarbage, True, count, levels)
  where (endOfGarbage, valid, count, levels) = anythingExceptGt xs nesting

anythingExceptGt :: ParseRule
anythingExceptGt ('!':x:xs) nesting = anythingExceptGt xs nesting
anythingExceptGt ('>':xs) nesting = ('>':xs, True, 0, [])
anythingExceptGt (x:xs) nesting = anythingExceptGt xs nesting


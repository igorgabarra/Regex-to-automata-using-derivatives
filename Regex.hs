module Regex where

import Prelude hiding (null)

-- definition of RE syntax

data RE a
   = Empty              -- empty RE
   | Epsilon            -- empty string
   | Chr a              -- single character RE
   | (RE a) :*: (RE a)  -- concatenation
   | (RE a) :|: (RE a)  -- union
   | Star (RE a)
   deriving (Eq, Ord, Show)

-- nullability tests

null :: RE a -> Bool
null Empty   = False
null Epsilon = True
null (Chr _) = False
null (e :*: e')
   = null e && null e'
null (e :|: e')
   = null e || null e'
null (Star _) = True

-- smart constructors to quotient REs.

infixl 4 :*:
infixl 3 :|:

(.|.) :: RE a -> RE a -> RE a
Empty .|. e' = e'
e .|. Empty  = e
e .|. e'     = e :|: e'

(.*.) :: RE a -> RE a -> RE a
Empty .*. e'   = Empty
e .*. Empty    = Empty
Epsilon .*. e' = e'
e .*. Epsilon  = e
e .*. e'       = e :*: e'

star :: RE a -> RE a
star Empty   = Epsilon
star Epsilon = Epsilon
star e       = Star e

-- Brzozowski derivative

deriv :: Eq a => RE a -> a -> RE a
deriv Empty _ = Empty
deriv Epsilon _ = Empty
deriv (Chr c) c'
   | c == c'   = Epsilon
   | otherwise = Empty
deriv (e :*: e') c
   | null e    = (deriv e c .*. e') .|. deriv e' c
   | otherwise = deriv e c .*. e'
deriv (e :|: e') c = deriv e c .|. deriv e' c
deriv (Star e) c = deriv e c .*. star e

-- prefix computation

data Result a
   = Result {
        consumed  :: [a]
     ,  remaining :: [a]
     }

cons :: a -> Result a -> Result a
cons x (Result cs rs) = Result (x : cs) rs

prefix :: Eq a => [a] -> RE a -> Maybe (Result a)
prefix [] e
   | null e
      = Just (Result [] [])
   | otherwise
      = Nothing
prefix (x : xs) e
   | null e
      = Just (Result [] (x : xs))
   | otherwise
      = cons x <$> prefix xs (deriv e x)

-- matching substrings

substring :: Eq a => [a] -> RE a -> Maybe (Result a)
substring [] e
   | null e
      = Just (Result [] [])
   | otherwise
      = Nothing
substring (x : xs) e
   = case prefix (x : xs) e of
      Just res -> Just res
      Nothing  -> cons x <$> substring xs e

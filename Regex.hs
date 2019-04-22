module Regex where

import Prelude hiding (null)
import Control.Monad
import Control.Applicative

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
Empty .|. e'        = e'
e .|. Empty         = e
e .|. e'            = e :|: e'
Epsilon .|. Empty   = Epsilon
Epsilon .|. Epsilon = Epsilon

(.*.) :: RE a -> RE a -> RE a
Empty .*. e'        = Empty
e .*. Empty         = Empty
Epsilon .*. Empty   = Empty
Epsilon .*. Epsilon = Epsilon
Epsilon .*. e'      = e'
e .*. Epsilon       = e
e .*. e'            = e :*: e'

star :: RE a -> RE a
star Empty   = Epsilon
star Epsilon = Epsilon
star e       = Star e

-- Brzozowski derivative

deriv :: Eq a => RE a -> a -> RE a
deriv Empty _      = Empty
deriv Epsilon _    = Empty
deriv (Chr c) c'
   | c == c'       = Epsilon
   | otherwise     = Empty
deriv (e :*: e') c
   | null e        = (deriv e c .*. e') .|. deriv e' c
   | otherwise     = deriv e c .*. e'
deriv (e :|: e') c = deriv e c .|. deriv e' c
deriv (Star e) c   = deriv e c .*. star e

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
   | null e    = Just (Result [] [])
   | otherwise = Nothing
prefix (x : xs) e
   | null e    = Just (Result [] (x : xs))
   | otherwise = cons x <$> prefix xs (deriv e x)

-- matching substrings

substring :: Eq a => [a] -> RE a -> Maybe (Result a)
substring [] e
   | null e    = Just (Result [] [])
   | otherwise = Nothing
substring (x : xs) e
               = case prefix (x : xs) e of
                      Just res -> Just res
                      Nothing  -> cons x <$> substring xs e

-------------- Working with lists --------------

newtype List a = List [a] deriving Eq

instance Ord a => Ord (List a) where
   List x <= List y = (length x, x) <= (length y, y)

type Word = List Char

instance Functor List where
   fmap f (List x) = List (fmap f x)

instance Applicative List where
   pure x = List [x]
   List x <*> List y = List (x <*> y)

instance Alternative List where
   empty = List []
   List x <|> List y = List (x <|> y)

instance Monad List where
   fs >>= f = joinn (f <$> fs) --what is <$>

union' :: Ord a => [a] -> [a] -> [a]
union' [] y = y
union' x [] = x
union' xa@(x:xs) ya@(y:ys) = case compare x y of
                             LT -> x : union' xs ya
                             EQ -> x : union' xs ys
                             GT -> y : union' xa ys

joinn :: List (List a) -> List a
joinn (List []) = List []
joinn (List (x:xs)) = lcat x (joinn (List xs))

crossProduct :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
crossProduct _ [] _ = []
crossProduct _ _ [] = []
crossProduct f (x:xs) ya@(y:ys)
   = f x y : (crossProduct f [x] ys) `union'` (crossProduct f xs ya)

lcat :: List a -> List a -> List a
lcat (List x) (List y) = List (x ++ y)

concatenation :: Ord a => [List a] -> [List a] -> [List a]
concatenation = crossProduct lcat

kleeneClosure :: Ord a => (a -> a -> a) -> a -> [a] -> [a]
kleeneClosure _ x [] = [x]
kleeneClosure f x ya@(y:ys)
   | x == y    = kleeneClosure f x ys
   | otherwise = x : crossProduct f ya (kleeneClosure f x ya)

clo :: Ord a => [List a] -> [List a]
clo = kleeneClosure lcat (List [])

enumRegularExpression :: Ord a => RE a -> [[a]]
enumRegularExpression exp = [w | (List w) <- enumRE exp]

enumRE :: Ord a => RE a -> [List a]
enumRE Empty      = []
enumRE Epsilon    = [List []]
enumRE (Chr a)    = [List [a]]
enumRE (exp1 :*: exp2)
                  = concatenation (enumRE exp1) (enumRE exp2)
enumRE (exp1 :|: exp2)
                  = enumRE exp1 `union'` enumRE exp2
enumRE (Star exp) = clo (enumRE exp)


-- Making Regular Expressions Trees

data Tree a
  = TEps
  | TChar a
  | TPair (Tree a) (Tree a)
  | TLeft (Tree a)
  | TRight (Tree a)
  | TNil
  | TCons (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

mkEps :: RE a -> Maybe (Tree a)
mkEps (Star _)
  = return TNil
mkEps (e :*: e')
  = TPair <$> mkEps e <*> mkEps e'
mkEps (e :|: e')
  | null e = TLeft <$> mkEps e
  | otherwise = TRight <$> mkEps e'
mkEps Epsilon = Just TEps 
mkEps _ = Nothing

inj :: Eq a => RE a -> a -> Tree a -> Maybe (Tree a)
inj (Chr a) a' TEps
  | a == a' = Just (TChar a)
  | otherwise = Nothing
inj (e1 :|: _) a (TLeft t1)
  = TLeft <$> inj e1 a t1
inj (_ :|: e2) a (TRight t2)
  = TRight <$> inj e2 a t2
inj (e1 :*: _) a (TPair t1 t2)
  = flip TPair t2 <$> inj e1 a t1
inj (e1 :*: _) a (TLeft (TPair t1 t2))
  = flip TPair t2 <$> inj e1 a t1
inj (e1 :*: e2) a (TRight t2)
  = TPair <$> mkEps e1 <*> inj e2 a t2
inj (Star e) a (TPair t1 t2)
  = flip TCons t2 <$> inj e a t1
inj _ _ _ = Nothing

dParse :: Eq a => RE a -> [a] -> Maybe (Tree a)
dParse e []
  | null e = mkEps e
  | otherwise = Nothing
dParse e (x : xs)
  =  maybe Nothing
           (inj e x)
           (dParse (deriv e x) xs)


first :: RE a -> RE a
--first Empty = Empty
--first lambda = nullable
first a = a
first e :|: e' = first e :|: first e'
--first e :*: e' 
--if lambda belongs to e then first e :|: first e'
--otherwise                   first e
first e :*: e'
      | belongs (e :*: e') = first e :|: first e'
      | otherwise          = first e
first (Star e') = first e'


belongs :: RE a -> Bool
belongs Empty    = True
belongs Epsilon  = False
belongs Chr e    = False
belongs e :*: e' = belongs e || belongs e'
belongs e :|: e' = belongs e || belongs e'
belongs Star e   = belongs e


solving :: RE a -> (RE a, RE a)
solving regex = (deriv regex (first regex), first regex)

memoryWords :: [RE a]
memoryWords = []

--calculating regex memory = case solving regex of
--                      (x, xs) -> do  newMemory <- x  
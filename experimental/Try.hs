{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE AutoDeriveTypeable    #-}

import           GHC.Exts         (Constraint)
import qualified Data.Map         as M
import           Text.Parsec
import Data.Typeable
import Data.Maybe (fromMaybe)

import GHC.TypeLits (Nat)

type f ==> g = (forall (n :: Nat). f n -> g n)

class HFunctor (h :: (Nat -> *) -> (Nat -> *)) where
  hfmap :: f ==> g -> h f ==> h g

data Fix (fs :: [(Nat -> *) -> (Nat -> *)]) (l :: Nat) where
 In :: HFunctor f => Elem f fs -> f (Fix fs) l -> Fix fs l
 deriving Typeable

data Elem (f :: (Nat -> *) -> (Nat -> *)) (fs :: [(Nat -> *) -> (Nat -> *)]) where
  Here :: Elem f (f ': fs)
  There :: Elem f fs -> Elem f (g ': fs)

class Belongs f fs where
  witness :: Elem f fs

instance {-# OVERLAPS #-} Belongs f (f ': fs) where
  witness = Here

instance {-# OVERLAPS #-} Belongs f fs => Belongs f (g ': fs) where
  witness = There witness

data Sub (fs :: [(Nat -> *) -> (Nat -> *)]) (gs::[(Nat -> *) -> (Nat -> *)]) where
  SNil :: Sub '[] gs
  SCons :: (HFunctor f) => Elem f gs -> Sub fs gs -> Sub (f ': fs) gs

class fs :< gs where
  srep :: Sub fs gs

instance '[] :< gs where
  srep = SNil

instance (HFunctor f, Belongs f gs, fs :< gs) => (f ': fs) :< gs where
  srep = SCons witness srep

data Classy (c :: ((Nat -> *) -> (Nat -> *)) -> Constraint) (fs :: [(Nat -> *) -> (Nat -> *)]) where
  CVoid :: Classy c fs
  CCons :: (HFunctor f, c f) => Classy c fs -> Classy c (f ': fs)

type Syntactic = Classy Syntax

type BoundContext = ([(Int, Int)], M.Map Int Int)

type Parser = Parsec String BoundContext

class HFunctor f => Syntax f where
  parseF :: Elem f fs -> TList fs -> TList fs

parseL' :: Sub fs fs -> Sub gs fs -> Syntactic fs -> Syntactic gs -> TList fs
parseL' r (SCons e SNil) o (CCons CVoid)  = parseF e (parseLang r o)
parseL' r (SCons e sub) o (CCons s) =
  let xs = parseF e (parseLang r o) in
  let ys = parseL' r sub o s in
  zipTList xs ys

parseLang :: Sub fs fs -> Syntactic fs -> TList fs
parseLang srep o = parseL' srep srep o o

parseL :: (fs :< fs) => Syntactic fs -> TList fs
parseL = parseLang srep


data ISig e l where
  Const :: Int -> ISig e 0
  Mult  :: e 0 -> e 0 -> ISig e 0
  Fst :: e 1 -> ISig e 0
  Snd :: e 1 -> ISig e 0

data PSig e l where
  Pair :: e 0 -> e 0 -> PSig e 1

instance HFunctor ISig where
  hfmap _ (Const i) = Const i
  hfmap f (Mult x y) = Mult (f x) (f y)
  hfmap f (Fst x) = Fst (f x)
  hfmap f (Snd x) = Snd (f x)

instance HFunctor PSig where
  hfmap f (Pair x y) = Pair (f x) (f y)

parseISig :: Elem ISig fs -> TList fs -> TList fs
parseISig e ps = (num e <|> do
  string "fst "
  expr <- ps !!! (1, (fail "") :: Parser (Fix fs 1))
  return $ In e (Fst expr)) ::: (fail "" :: Parser (Fix fs 1)) ::: TNil

instance Syntax ISig where
  parseF = parseISig

num :: Elem ISig fs -> Parser (Fix fs 0)
num e = do
  n <- many1 digit
  return (In e (Const (read n)))

instance Syntax PSig where
  parseF = parsePSig

parsePSig :: Elem PSig fs -> TList fs -> TList fs
parsePSig e ps = (fail "" :: Parser (Fix fs 0)) ::: (do
  char '('
  e1 <- ps !!! (0, (fail "") :: Parser (Fix fs 0))
  char ','
  e2 <- ps !!! (0, (fail "") :: Parser (Fix fs 0))
  char ')'
  return (In e (Pair e1 e2))) ::: TNil

s :: Syntactic '[ISig, PSig]
s = CCons (CCons CVoid)

t1 = (parseL s) !!! (0, (fail "") :: Parser (Fix fs 0))
t2 = (parseL s) !!! (1, (fail "") :: Parser (Fix fs 1))

run1 p = putStrLn $ p ++ "\t => \t" ++ p'
  where
    p' = case runParser t1 ([], M.empty) "Test" p of
           Left _ -> "run1 WRONG"
           Right e -> "run1 RIGHT"

run2 p = putStrLn $ p ++ "\t => \t" ++ p'
  where
    p' = case runParser t2 ([], M.empty) "Test" p of
           Left _ -> "run2 WRONG"
           Right e -> "run2 RIGHT"

-- Typed Lists

data TList fs where
  TNil  :: TList fs
  (:::) :: Typeable t => Parser (Fix fs t) -> TList fs -> TList fs

infixr 7 :::
infixl 9 !!!

zipTList :: TList fs -> TList fs -> TList fs
zipTList TNil TNil = TNil
zipTList a@(x ::: xs) b@(y ::: ys) = (x <|> (b !!! (0, x))) ::: (zipTList xs ys)

(!!!) :: Typeable t => TList fs -> (Int, Parser (Fix fs t)) -> Parser (Fix fs t)
TNil !!! _ = fail ""
(x ::: xs) !!! (i, t)
  | i < 0 = fail ""
  | i == 0 = fromMaybe (fail "") $ myCast t x
  | otherwise = xs !!! (i - 1, t)
  
myCast :: (Typeable a, Typeable b) => Parser (Fix fs a) -> Parser (Fix fs b) -> Maybe (Parser (Fix fs a))
myCast x y = help x (myGCast y)

myGCast :: forall fs a b c. (Typeable a, Typeable b) => Parser (Fix fs a) -> Maybe (Parser (Fix fs b))
myGCast x = fmap (\Refl -> x) (eqT :: Maybe (a :~: b))

help :: a -> Maybe a -> Maybe a
help x y = y

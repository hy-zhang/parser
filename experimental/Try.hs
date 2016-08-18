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

import           GHC.Exts         (Constraint)
import           GHC.TypeLits
import qualified Data.Map         as M
import           Text.Parsec
import           Data.Typeable
import           Data.Maybe       (fromJust)
import           Data.List        (findIndex)

-- High-order functors

type f ==> g = (forall (n :: Nat). f n -> g n)

class HFunctor (h :: (Nat -> *) -> (Nat -> *)) where
  hfmap :: f ==> g -> h f ==> h g
  
-- MRM code: Fix, Elem 

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

-- Sub: list of h-functors

data Sub (fs :: [(Nat -> *) -> (Nat -> *)]) (gs :: [(Nat -> *) -> (Nat -> *)]) where
  SNil :: Sub '[] gs
  SCons :: (HFunctor f) => Elem f gs -> Sub fs gs -> Sub (f ': fs) gs

class fs :< gs where
  srep :: Sub fs gs

instance '[] :< gs where
  srep = SNil

instance (HFunctor f, Belongs f gs, fs :< gs) => (f ': fs) :< gs where
  srep = SCons witness srep

-- NSub :: list of typelits

data NSub (ms :: [Nat]) (ns :: [Nat]) where
  NNil :: NSub '[] ns
  NCons :: KnownNat m => Proxy m -> NSub ms ns -> NSub (m ': ms) ns

class ms :> ns where
  nrep :: NSub ms ns

instance '[] :> ns where
  nrep = NNil

instance (ms :> ns, KnownNat m) => (m ': ms) :> ns where
  nrep = NCons Proxy nrep

-- Classy, Syntax, Syntactic, Parser: features of the parser

data Classy (c :: ((Nat -> *) -> (Nat -> *)) -> Nat -> Constraint) (fs :: [(Nat -> *) -> (Nat -> *)]) (ns :: [Nat]) where
  CVoid :: Classy c fs ns
  CCons :: (HFunctor f, c f n) => Classy c fs ns -> Classy c (f ': fs) (n ': ns)

class HFunctor f => Syntax f (n :: Nat) where -- n not needed? (TODO)
  parseF :: Proxy n -> Elem f fs -> TList fs -> Parser (Fix fs n)

type Syntactic = Classy Syntax

type BoundContext = ([(Int, Int)], M.Map Int Int)

type Parser = Parsec String BoundContext

parseL' :: Sub fs fs -> Sub gs fs -> NSub ms ms -> NSub ns ms -> Syntactic fs ms -> Syntactic gs ns -> TList fs
parseL' r (SCons e SNil) n (NCons p NNil) o (CCons CVoid) = parseF p e (parseLang r n o) ::: TNil
parseL' r (SCons e sub) n (NCons p nsub) o (CCons s) = tAddParser (parseF p e (parseLang r n o)) (parseL' r sub n nsub o s)

parseLang :: Sub fs fs -> NSub ms ms -> Syntactic fs ms -> TList fs
parseLang srep nrep o = parseL' srep srep nrep nrep o o

parseL :: (fs :< fs, ms :> ms) => Syntactic fs ms -> TList fs
parseL = parseLang srep nrep

-- Typed lists

data TList fs where
  TNil  :: TList fs
  (:::) :: (Typeable t, KnownNat t) => Parser (Fix fs t) -> TList fs -> TList fs

infixr 7 :::
infixl 9 !!!

-- Library functions for typed lists

zipTList :: TList fs -> TList fs -> TList fs
zipTList TNil TNil = TNil
zipTList a@(x ::: xs) b@(y ::: ys) = (x <|> (tGet b (0, getProxy x))) ::: (zipTList xs ys)

(!!!) :: (Typeable t, KnownNat t) => TList fs -> Proxy t -> Parser (Fix fs t)
l !!! t = tGet l (nat2Int t, t) -- '[0, 2, 4, 7]: use find instead (TODO)

tGet :: Typeable t => TList fs -> (Int, Proxy t) -> Parser (Fix fs t)
tGet TNil _ = undefined
tGet (x ::: xs) (i, t)
  | i < 0 = undefined
  | i == 0 = fromJust $ myCast (fail "" :: Parser (Fix fs t)) x
  | otherwise = tGet xs (i - 1, t)

tInsert :: KnownNat t => Int -> Parser (Fix fs t) -> TList fs -> TList fs
tInsert _ p TNil = p ::: TNil
tInsert 0 p xs = p ::: xs
tInsert i p (x ::: xs) = x ::: tInsert (i - 1) p xs

tReplace :: KnownNat t => Int -> Parser (Fix fs t) -> TList fs -> TList fs
tReplace _ _ TNil = TNil
tReplace 0 p (x ::: xs) = p ::: xs
tReplace i p (x ::: xs) = x ::: tReplace (i - 1) p xs

tAddParser :: (Typeable t, KnownNat t) => Parser (Fix fs t) -> TList fs -> TList fs
tAddParser p xs = case b of
  True -> tReplace k (p <|> (tGet xs (k, Proxy :: Proxy t))) xs
  False -> tInsert k p xs
  where (k, b) = tAddParserAux p xs

tAddParserAux :: KnownNat t => Parser (Fix fs t) -> TList fs -> (Int, Bool)
tAddParserAux p xs = case m of
  Just k -> (k, ns !! k == n)
  Nothing -> (length ns, False)
  where
    n = parser2Int p
    ns = getNums xs
    m = findIndex (>= n) ns
  
myCast :: (Typeable a, Typeable b) => Parser (Fix fs a) -> Parser (Fix fs b) -> Maybe (Parser (Fix fs a))
myCast x y = help x (myGCast y)

myGCast :: forall fs a b c. (Typeable a, Typeable b) => Parser (Fix fs a) -> Maybe (Parser (Fix fs b))
myGCast x = fmap (\Refl -> x) (eqT :: Maybe (a :~: b))

help :: a -> Maybe a -> Maybe a
help x y = y

getProxy :: Parser (Fix fs t) -> Proxy t
getProxy _ = Proxy

nat2Int :: KnownNat n => Proxy n -> Int
nat2Int = fromIntegral . natVal

parser2Int :: KnownNat n => Parser (Fix fs n) -> Int
parser2Int p = parser2IntAux p Proxy

parser2IntAux :: KnownNat n => Parser (Fix fs n) -> Proxy n -> Int
parser2IntAux _ p = fromIntegral (natVal p)

getNums :: TList fs -> [Int]
getNums TNil = []
getNums (x ::: xs) = parser2Int x : getNums xs

-- CLIENT CODE BELOW

-- Datatype: ISig

-- 1. Datatype declaration
data ISig e l where
  Const :: Int -> ISig e 0
  Mult  :: e 0 -> e 0 -> ISig e 0
  Fst :: e 1 -> ISig e 0
  Snd :: e 1 -> ISig e 0

-- 2. HFunctor instance
instance HFunctor ISig where
  hfmap _ (Const i) = Const i
  hfmap f (Mult x y) = Mult (f x) (f y)
  hfmap f (Fst x) = Fst (f x)
  hfmap f (Snd x) = Snd (f x)

-- 3. Parser implementation
parseISig :: Elem ISig fs -> TList fs -> Parser (Fix fs 0)
parseISig e ps = num e <|> do
  string "fst "
  expr <- ps !!! (Proxy :: Proxy 1)
  return $ In e (Fst expr)

num :: Elem ISig fs -> Parser (Fix fs 0)
num e = do
  n <- many1 digit
  return (In e (Const (read n)))

-- 4. Syntax instance
instance Syntax ISig 0 where
  parseF _ = parseISig

-- Datatype: PSig

-- 1. Datatype declaration
data PSig e l where
  Pair :: e 0 -> e 0 -> PSig e 1

-- 2. HFunctor instance
instance HFunctor PSig where
  hfmap f (Pair x y) = Pair (f x) (f y)

-- 3. Parser implementation
parsePSig :: Elem PSig fs -> TList fs -> Parser (Fix fs 1)
parsePSig e ps = do
  char '('
  e1 <- ps !!! (Proxy :: Proxy 0)
  char ','
  e2 <- ps !!! (Proxy :: Proxy 0)
  char ')'
  return (In e (Pair e1 e2))

-- 4. Syntax instance
instance Syntax PSig 1 where
  parseF _ = parsePSig

-- Testing

s :: Syntactic '[ISig, PSig] '[0, 1]
s = CCons (CCons CVoid)

t1 = (parseL s) !!! (Proxy :: Proxy 0)
t2 = (parseL s) !!! (Proxy :: Proxy 1)

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

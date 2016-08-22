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
import           Data.Maybe       (fromJust, fromMaybe)
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

-- Sub: list of h-functors & typelits

data Sub (fs :: [(Nat -> *) -> (Nat -> *)]) (gs :: [(Nat -> *) -> (Nat -> *)]) (ms :: [Nat]) (ns :: [Nat]) where
  SNil :: Sub '[] gs '[] ns
  SCons :: (HFunctor f, KnownNat m, Typeable m) => Elem f gs -> Proxy m -> Sub fs gs ms ns -> Sub (f ': fs) gs (m ': ms) ns

class Subrep fs gs ms ns where
  srep :: Sub fs gs ms ns

instance Subrep '[] gs '[] ns where
  srep = SNil

instance (HFunctor f, Belongs f gs, KnownNat m, Typeable m, Subrep fs gs ms ns) => Subrep (f ': fs) gs (m ': ms) ns where
  srep = SCons witness Proxy srep

-- Classy, Syntax, Syntactic, Parser: features of the parser

data Classy (c :: ((Nat -> *) -> (Nat -> *)) -> Nat -> Constraint) (fs :: [(Nat -> *) -> (Nat -> *)]) (ns :: [Nat]) where
  CVoid :: Classy c fs ns
  CCons :: (HFunctor f, c f n) => Classy c fs ns -> Classy c (f ': fs) (n ': ns)

class HFunctor f => Syntax f (n :: Nat) where -- n not needed? (TODO)
  parseF :: Proxy n -> Elem f fs -> TList fs -> Parser (Fix fs n)

type Syntactic = Classy Syntax

type BoundContext = ([(Int, Int)], M.Map Int Int)

type Parser = Parsec String BoundContext

parseL' :: Sub fs fs ms ms -> Sub gs fs ns ms -> Syntactic fs ms -> Syntactic gs ns -> TList' fs
parseL' r (SCons e p SNil) o (CCons CVoid) = [parseF p e (parseLang r o)] :::: TNil'
parseL' r (SCons e p sub) o (CCons s) = tAddParser (parseF p e (parseLang r o)) (parseL' r sub o s)

parseLang :: Sub fs fs ms ms -> Syntactic fs ms -> TList fs
parseLang srep o = trans $ parseL' srep srep o o

parseL :: Subrep fs fs ms ms => Syntactic fs ms -> TList fs
parseL = parseLang srep

-- Typed lists

data TList fs where
  TNil  :: TList fs
  (:::) :: (Typeable t, KnownNat t) => Parser (Fix fs t) -> TList fs -> TList fs

data TList' fs where
  TNil'  :: TList' fs
  (::::) :: (Typeable t, KnownNat t) => [Parser (Fix fs t)] -> TList' fs -> TList' fs

infixr 7 ::::
infixr 7 :::
infixl 9 !!!

trans :: TList' fs -> TList fs
trans TNil' = TNil
trans (xs :::: xss) = foldl1 (<|>) xs ::: trans xss

tAddParser :: (Typeable t, KnownNat t) => Parser (Fix fs t) -> TList' fs -> TList' fs
tAddParser p TNil' = [p] :::: TNil'
tAddParser p (xs :::: xss) =
  let mb = myCast p (head xs) in
  case mb of
    Nothing -> xs :::: tAddParser p xss
    Just _  -> (map (fromJust . myCast p) xs) :::: xss

-- Library functions for typed lists

(!!!) :: (Typeable t, KnownNat t) => TList fs -> Proxy t -> Parser (Fix fs t)
TNil !!! t = error $ "Parser " ++ show (nat2Int t) ++ " not found."
(x ::: xs) !!! t =
  let mb = myCast (fail "" :: Parser (Fix fs t)) x in
  case mb of
    Just p  -> p
    Nothing -> xs !!! t
  
myCast :: (Typeable a, Typeable b) => Parser (Fix fs a) -> Parser (Fix fs b) -> Maybe (Parser (Fix fs a))
myCast x y = help x (myGCast y)
  where
    myGCast :: forall fs a1 b1 c1. (Typeable a1, Typeable b1) => Parser (Fix fs a1) -> Maybe (Parser (Fix fs b1))
    myGCast x = fmap (\Refl -> x) (eqT :: Maybe (a1 :~: b1))
    help :: e -> Maybe e -> Maybe e
    help x y = y

getProxy :: Parser (Fix fs t) -> Proxy t
getProxy _ = Proxy

nat2Int :: KnownNat n => Proxy n -> Int
nat2Int = fromIntegral . natVal

parser2Int :: KnownNat n => Parser (Fix fs n) -> Int
parser2Int p = parser2IntAux p Proxy
  where
    parser2IntAux :: KnownNat m => Parser (Fix fs m) -> Proxy m -> Int
    parser2IntAux _ q = fromIntegral (natVal q)

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

                              


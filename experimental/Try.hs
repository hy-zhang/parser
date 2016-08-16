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

import           GHC.Exts         (Constraint)
import qualified Data.Map         as M
import           Text.Parsec

type f ==> g = forall a. f a -> g a

class HFunctor h where
  hfmap :: f ==> g -> h f ==> h g

data Fix (fs :: [(* -> *) -> (* -> *)]) l where
 In :: HFunctor f => Elem f fs -> f (Fix fs) l -> Fix fs l

data Elem (f :: (* -> *) -> (* -> *)) (fs :: [(* -> *) -> (* -> *)]) where
  Here :: Elem f (f ': fs)
  There :: Elem f fs -> Elem f (g ': fs)

class Belongs f fs where
  witness :: Elem f fs

instance {-# OVERLAPS #-} Belongs f (f ': fs) where
  witness = Here

instance {-# OVERLAPS #-} Belongs f fs => Belongs f (g ': fs) where
  witness = There witness

data Sub (fs :: [(* -> *) -> (* -> *)]) (gs::[(* -> *) -> (* -> *)]) where
  SNil :: Sub '[] gs
  SCons :: (HFunctor f) => Elem f gs -> Sub fs gs -> Sub (f ': fs) gs

class fs :< gs where
  srep :: Sub fs gs

instance '[] :< gs where
  srep = SNil

instance (HFunctor f, Belongs f gs, fs :< gs) => (f ': fs) :< gs where
  srep = SCons witness srep

data Classy (c :: ((* -> *) -> (* -> *)) -> Constraint) (fs :: [(* -> *) -> (* -> *)]) where
  CVoid :: Classy c fs
  CCons :: (HFunctor f, c f) => Classy c fs -> Classy c (f ': fs)

type Syntactic = Classy Syntax

type BoundContext = ([(Int, Int)], M.Map Int Int)

type Parser = Parsec String BoundContext

class HFunctor f => Syntax f where
  parseF :: Elem f fs -> (Parser (Fix fs A), Parser (Fix fs B)) -> (Parser (Fix fs A), Parser (Fix fs B))

parseL' :: Sub fs fs -> Sub gs fs -> Syntactic fs -> Syntactic gs -> (Parser (Fix fs A), Parser (Fix fs B))
parseL' r (SCons e SNil) o (CCons CVoid)  = parseF e (parseLang r o)
parseL' r (SCons e sub) o (CCons s) = 
  let (p1, p2) = parseF e (parseLang r o) in
  let (p3, p4) = parseL' r sub o s in
  (p1 <|> p3, p2 <|> p4)

parseLang :: Sub fs fs -> Syntactic fs -> (Parser (Fix fs A), Parser (Fix fs B))
parseLang srep o = parseL' srep srep o o

parseL :: (fs :< fs) => Syntactic fs -> (Parser (Fix fs A), Parser (Fix fs B))
parseL = parseLang srep

data A
data B

data ISig (e :: * -> *) l where
  Const :: Int -> ISig e A
  Mult  :: e A -> e A -> ISig e A
  Fst, Snd :: e B -> ISig e A

data PSig (e :: * -> *) l where
  Pair :: e A -> e A -> PSig e B

instance HFunctor ISig where
  hfmap _ (Const i) = Const i
  hfmap f (Mult x y) = Mult (f x) (f y)
  hfmap f (Fst x) = Fst (f x)
  hfmap f (Snd x) = Snd (f x)

instance HFunctor PSig where
  hfmap f (Pair x y) = Pair (f x) (f y)

instance Syntax ISig where
  parseF = parseISig

parseISig :: Elem ISig fs -> (Parser (Fix fs A), Parser (Fix fs B)) -> (Parser (Fix fs A), Parser (Fix fs B))
parseISig e (p1, p2) = (num e <|> do
  string "fst "
  expr <- p2
  return (In e (Fst expr)), fail "")

num :: Elem ISig fs -> Parser (Fix fs A)
num e = do
  n <- many1 digit
  return (In e (Const (read n)))

instance Syntax PSig where
  parseF = parsePSig

parsePSig :: Elem PSig fs -> (Parser (Fix fs A), Parser (Fix fs B)) -> (Parser (Fix fs A), Parser (Fix fs B))
parsePSig e (p1, p2) = (fail "", do
  char '('
  e1 <- p1
  char ','
  e2 <- p1
  char ')'
  return (In e (Pair e1 e2)))

s :: Syntactic '[ISig, PSig]
s = CCons (CCons CVoid)

t1 = fst (parseL s)
t2 = snd (parseL s)

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
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

module Lib (
  Fix(..), Elem, Syntactic, Syntax(..), Gen(..),
  TList, (!!!), Parser, parseL, mapFst, mapSnd,
  NewParser, OneParser, num, keyword, parseWord,
  checkR, resetR, chainlR, choiceR
) where

import           GHC.Exts             (Constraint)
import           GHC.TypeLits
import           Control.Monad        (mzero)
import qualified Data.Map             as M
import qualified Text.Parsec.Token    as T
import           Text.Parsec          hiding (runP)
import           Text.PrettyPrint     hiding (char, parens, space)
import           Text.Parsec.Language (emptyDef)
import           Data.Typeable
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.List            (findIndex)


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
  SCons :: (HFunctor f, KnownNat m, Typeable m) => Proxy m -> Elem f gs -> Sub fs gs ms ns -> Sub (f ': fs) gs (m ': ms) ns

class Subrep fs gs ms ns where
  srep :: Sub fs gs ms ns

instance Subrep '[] gs '[] ns where
  srep = SNil

instance (HFunctor f, Belongs f gs, KnownNat m, Typeable m, Subrep fs gs ms ns) => Subrep (f ': fs) gs (m ': ms) ns where
  srep = SCons Proxy witness srep

-- Classy, Syntax, Syntactic, Parser: features of the parser

data Classy (c :: ((Nat -> *) -> (Nat -> *)) -> Nat -> Constraint) (fs :: [(Nat -> *) -> (Nat -> *)]) (ns :: [Nat]) where
  CVoid :: Classy c fs ns
  CCons :: (HFunctor f, c f n) => Classy c fs ns -> Classy c (f ': fs) (n ': ns) -- Subrep needed?

class Gen fs ms where
 crep :: Syntactic fs ms

instance Gen '[] '[] where
 crep = CVoid

instance (Syntax f m, Gen fs ms) => Gen (f ': fs) (m ': ms) where
 crep = CCons crep

class HFunctor f => Syntax f (n :: Nat) where
  level :: Proxy n -> Elem f fs -> Int
  level _ _ = 0
  keywords :: Proxy n -> Elem f fs -> [String]
  keywords _ _ = []
  parseF :: Proxy n -> Elem f fs -> TList fs -> Parser (Fix fs n)

type Syntactic = Classy Syntax

data ParserContext = ParserContext
                       { stk :: [(Int, Int)]
                       , mp  :: M.Map Int Int
                       , kws :: [String]
                       }

type Parser = Parsec String ParserContext

parseL' :: Sub fs fs ms ms -> Sub gs fs ns ms -> Syntactic fs ms -> Syntactic gs ns -> TList' fs
parseL' r (SCons p e SNil) o (CCons CVoid) = [(try $ parseF p e (parseLang r o), level p e)] :::: TNil'
parseL' r (SCons p e sub) o (CCons s) = tAddParser (try $ parseF p e (parseLang r o), level p e) (parseL' r sub o s)

parseLang :: Sub fs fs ms ms -> Syntactic fs ms -> TList fs
parseLang srep o = trans $ parseL' srep srep o o

parseL :: Subrep fs fs ms ms => Syntactic fs ms -> TList fs
parseL = parseLang srep

getKeywords :: Subrep fs fs ms ms => Syntactic fs ms -> [String]
getKeywords o = ttt srep o o
  where
    ttt :: Sub gs fs ns ms -> Syntactic fs ms -> Syntactic gs ns -> [String]
    ttt SNil          o CVoid      = []
    ttt (SCons p e sub) o (CCons cs) = keywords p e ++ ttt sub o cs

-- Typed lists

data TList fs where
  TNil  :: TList fs
  (:::) :: (Typeable t, KnownNat t) => Parser (Fix fs t) -> TList fs -> TList fs

data TList' fs where
  TNil'  :: TList' fs
  (::::) :: (Typeable t, KnownNat t) => [(Parser (Fix fs t), Int)] -> TList' fs -> TList' fs

infixr 7 ::::
infixr 7 :::
infixl 9 !!!

trans :: TList' fs -> TList fs
trans TNil' = TNil
trans (xs :::: xss) = f xs ::: trans xss
  where
    sort [] = []
    sort (p : ps) = insert p (sort ps)
    insert p [] = [p]
    insert p (q : qs) = if snd p > snd q then p : q : qs else q : insert p qs
    f = foldl1 (<|>) . map fst . sort

tAddParser :: (Typeable t, KnownNat t) => (Parser (Fix fs t), Int) -> TList' fs -> TList' fs
tAddParser q TNil' = [q] :::: TNil'
tAddParser q@(p, n) (xs :::: xss) =
  let mb = myCast p (fst $ head xs) in
  case mb of
    Nothing -> xs :::: tAddParser q xss
    Just _  -> (q : map (mapFst (fromJust . myCast p)) xs) :::: xss

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

-- Other library functions

type NewParser f fs n = Elem f fs -> TList fs -> Parser (Fix fs n)

type OneParser f fs n = Elem f fs -> Parser (Fix fs n) -> Parser (Fix fs n)

checkR :: OneParser f fs n
checkR e p = let ex = calc e in do
  x <- getMark e
  state <- getState
  if (ex, x) `elem` stk state
    then fail ""
    else modifyState (\s -> let t = stk s in s {stk = (ex, x) : t}) >> p

resetR :: Elem f fs -> Parser ()
resetR e = do
  x <- getMark e
  let ex = calc e
      f (y1, y2) = y1 > ex || (y1 == ex && y2 >= x)
  modifyState (\s -> let t = stk s in s {stk = dropWhile f t})

chainlR :: HFunctor f => Parser t -> (Fix fs n -> t -> f (Fix fs) n) -> OneParser f fs n
chainlR parser ctr e p = do
  e1 <- checkR e p
  (do xs <- many1 parser
      resetR e
      return $ foldl (\acc -> In e . ctr acc) e1 xs) <|> (resetR e >> pure e1)

choiceR :: Elem f fs -> [Parser (Fix fs n)] -> Parser (Fix fs n)
choiceR _ [] = mzero
choiceR e xs = foldl1 (<|>) . zipWith (\i x -> f i >> x) [1..] $ xs
  where f i = modifyState (\s -> let m = mp s in s {mp = M.insert (calc e) i m})

getMark :: Elem f fs -> Parser Int
getMark e = do
  state <- getState
  return . fromMaybe 1 $ M.lookup (calc e) (mp state)

calc :: Elem f fs -> Int
calc Here = 1
calc (There e) = 1 + calc e

num :: Parser Int
num = do n <- many1 digit
         return $ read n

keyword :: String -> Parsec String ParserContext ()
keyword s = try $ spaces >> string s >> spaces

parseWord :: Parsec String ParserContext String
parseWord = do
  w <- many1 (letter <|> char '\'')
  state <- getState
  if w `elem` kws state
    then unexpected $ show w ++ "; It cannot be used as a var because it's a predefined keyword."
    else return w

-- Parentheses are no longer implicitly supported.
parseBase :: Parser (Fix fs n) -> Parser (Fix fs n)
parseBase p = do
  _ <- char '('
  state <- getState
  modifyState $ \s -> s {stk = []}
  x <- p
  _ <- char ')'
  modifyState $ \s -> s {stk = stk state}
  return x

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

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
parseISig :: NewParser ISig fs 0
parseISig e ps = constNum e <|> do
  string "fst "
  expr <- ps !!! (Proxy :: Proxy 1)
  return $ In e (Fst expr)

constNum :: Elem ISig fs -> Parser (Fix fs 0)
constNum e = do
  n <- many1 digit
  return (In e (Const (read n)))

-- 4. Syntax instance
instance Syntax ISig 0 where
  parseF _  = parseISig
  level _ _ = 10

-- Datatype: PSig

-- 1. Datatype declaration
data PSig e l where
  Pair :: e 0 -> e 0 -> PSig e 1

-- 2. HFunctor instance
instance HFunctor PSig where
  hfmap f (Pair x y) = Pair (f x) (f y)

-- 3. Parser implementation
parsePSig :: NewParser PSig fs 1
parsePSig e ps = do
  char '('
  e1 <- ps !!! (Proxy :: Proxy 0)
  char ','
  e2 <- ps !!! (Proxy :: Proxy 0)
  char ')'
  return (In e (Pair e1 e2))

-- 4. Syntax instance
instance Syntax PSig 1 where
  parseF _  = parsePSig
  level _ _ = 5

-- Testing

s :: Syntactic '[ISig, PSig] '[0, 1]
s = crep

t1 = (parseL s) !!! (Proxy :: Proxy 0)
t2 = (parseL s) !!! (Proxy :: Proxy 1)

-- run1 p = putStrLn $ p ++ "\t => \t" ++ p'
  -- where
    -- p' = case runParser t1 ([], M.empty) "Test" p of
           -- Left _ -> "run1 WRONG"
           -- Right e -> "run1 RIGHT"
           

-- run2 p = putStrLn $ p ++ "\t => \t" ++ p'
  -- where
    -- p' = case runParser t2 ([], M.empty) "Test" p of
           -- Left _ -> "run2 WRONG"
           -- Right e -> "run2 RIGHT"

-- r = sequence_ [run1 "123", run1 "(12,4)", run1 "fst (12,34)", run2 "123", run2 "(12,4)", run2 "fst (12,34)"]
                              


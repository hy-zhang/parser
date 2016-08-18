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

module Lib (
  Fix(..), Classy(..), Elem, Syntactic, Syntax(..), Gen(..), (:<)(..),
  Parser, parseL, prettyL, mapFst, mapSnd,
  NewParser, runP, runP', num, keyword, parseWord,
  checkR, resetR, chainlR, choiceR,
) where

import           Control.Monad        (mzero)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Debug.Trace          (trace)
import           GHC.Exts             (Constraint)
import           Text.Parsec          hiding (runP)
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as T
import           Text.PrettyPrint     hiding (char, parens, space)

-- GENERAL LIBRARY COMPONENTS --

data ParserContext = ParserContext
                       { stk :: [(Int, Int)]
                       , mp  :: M.Map Int Int
                       , kws :: [String]
                       }

-- type BoundContext = ([(Int, Int)], M.Map Int Int, [String]) -- Environment is currently not extensible.

type Parser = Parsec String ParserContext

data Fix (fs :: [* -> *]) where
 In :: Functor f => Elem f fs -> f (Fix fs) -> Fix fs

data Elem (f :: * -> *) (fs :: [* -> *]) where
 Here :: Elem f (f ': fs)
 There :: Elem f fs -> Elem f (g ': fs)

class Belongs f fs where
  witness :: Elem f fs

instance {-# OVERLAPS #-} Belongs f (f ': fs) where
  witness = Here

instance {-# OVERLAPS #-} Belongs f fs => Belongs f (g ':fs) where
  witness = There witness

inn :: (Belongs f fs, Functor f) => f (Fix fs) -> Fix fs
inn = In witness


data Matches (fs :: [* -> *]) (a :: *) (b :: *) where
  Void :: Matches '[] a b
  (:::) :: Functor f => (f a -> b) -> Matches fs a b -> Matches (f ': fs) a b

data Classy (c :: (* -> *) -> Constraint) (fs :: [* -> *]) where
 CVoid :: Classy c '[]
 CCons :: (fs :< fs, Functor f, c f) => Classy c fs -> Classy c (f ': fs)

type Syntactic = Classy Syntax

class Gen fs where
 crep :: Syntactic fs

instance Gen '[] where
 crep = CVoid

instance (fs :< fs, Syntax f, Gen fs) => Gen (f ': fs) where
 crep = CCons crep

data Sub (fs :: [* -> *]) (gs::[* -> *]) where
  SNil :: Sub '[] gs
  SCons :: (Functor f) => Elem f gs -> Sub fs gs -> Sub (f ': fs) gs

class fs :< gs where
  srep :: Sub fs gs

instance '[] :< gs where
  srep = SNil

instance (Functor f, Belongs f gs, fs :< gs) => (f ': fs) :< gs where
  srep = SCons witness srep


class Functor f => Syntax f where
  keywords :: Elem f fs -> [String]
  keywords = const []
  parseF :: Elem f fs -> Parser (Fix fs) -> Parser (Fix fs)
  prettyF :: (r -> Doc) -> f r -> Doc

parseL' :: Sub fs fs -> Sub gs fs -> Syntactic fs -> Syntactic gs -> Parser (Fix fs)
parseL' r (SCons e SNil) o (CCons CVoid)  = try (parseF e (parseLang r o)) <|> try (parseBase (parseLang r o))
parseL' r (SCons e sub) o (CCons s)       = try (parseF e (parseLang r o)) <|> parseL' r sub o s

parseLang :: Sub fs fs -> Syntactic fs -> Parser (Fix fs)
parseLang srep o = parseL' srep srep o o

parseL :: (fs :< fs) => Syntactic fs -> Parser (Fix fs)
parseL = parseLang srep

parseBase :: Parser (Fix fs) -> Parser (Fix fs)
parseBase p = do
  _ <- char '('
  state <- getState
  modifyState $ \s -> s {stk = []}
  -- modifyState $ mapFst (const [])
  x <- p
  _ <- char ')'
  modifyState $ \s -> s {stk = stk state}
  -- modifyState $ mapFst (const s)
  return x

prettyL' :: Elem g gs -> Syntactic gs -> Syntactic fs -> g (Fix fs) -> Doc
prettyL' Here        (CCons cs)  u  t = prettyF (prettyL u) t
prettyL' (There e')  (CCons cs)  u  t = prettyL' e' cs u t

prettyL :: Syntactic fs -> Fix fs -> Doc
prettyL u (In e t)  = prettyL' e u u t

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

runP :: (fs :< fs) => Syntactic fs -> String -> IO ()
runP s0 p = putStrLn $ p ++ "\t => \t" ++ p'
  where
    p' = case runParser (parseL s0) initState "Test" p of
           Left t -> show t
           Right e -> show (prettyL s0 e)
    initState = ParserContext [] M.empty (getKeywords s0)

runP' s p =
  case runParser (parseL s) initState "Test" p of
    Left t -> show t
    Right e -> show (prettyL s e)
  where
    initState = ParserContext [] M.empty (getKeywords s)

getKeywords :: (fs :< fs) => Syntactic fs -> [String]
getKeywords o = ttt srep o o
  where
    ttt :: Sub gs fs -> Syntactic fs -> Syntactic gs -> [String]
    ttt SNil          o CVoid      = []
    ttt (SCons e sub) o (CCons cs) = keywords e ++ ttt sub o cs

-- LIBRARY FUNCTIONS

type NewParser f fs = Elem f fs -> Parser (Fix fs) -> Parser (Fix fs)

checkR :: NewParser f fs
checkR e p = let ex = calc e in do
  x <- getMark e
  state <- getState
  if (ex, x) `elem` stk state
    then fail ""
    else modifyState (\s -> let t = stk s in s {stk = (ex, x) : t}) >> p

calc :: Elem f fs -> Int
calc Here = 1
calc (There e) = 1 + calc e

resetR :: Elem f fs -> Parser ()
resetR e = do
  x <- getMark e
  let ex = calc e
      f (y1, y2) = y1 > ex || (y1 == ex && y2 >= x)
  modifyState (\s -> let t = stk s in s {stk = dropWhile f t})

getMark :: Elem f fs -> Parser Int
getMark e = do
  state <- getState
  return . fromMaybe 1 $ M.lookup (calc e) (mp state)

chainlR :: Functor f => Parser t -> (Fix fs -> t -> f (Fix fs)) -> NewParser f fs
chainlR parser ctr e p = do
  e1 <- checkR e p
  (do xs <- many1 parser
      resetR e
      return $ foldl (\acc -> In e . ctr acc) e1 xs) <|> (resetR e >> pure e1)

choiceR :: Elem f fs -> [Parser (Fix fs)] -> Parser (Fix fs)
choiceR _ [] = mzero
choiceR e xs = foldl1 (<|>) . zipWith (\i x -> f i >> x) [1..] $ xs
  where f i = modifyState (\s -> let m = mp s in s {mp = M.insert (calc e) i m})

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

{-
-- Arith

data Arith r = Lit Int | Add r r | Sub r r deriving (Functor,Show)

parseArith :: Elem Arith fs -> Parser (Fix fs) -> Parser (Fix fs)
parseArith e p = choiceR e [
    chainlR (char '+' >> p) Add e p,
    chainlR (char '-' >> p) Sub e p,
    pure (In e . Lit) <*> num
  ]

instance Syntax Arith where
 parseF                 = parseArith
 prettyF r (Lit x)      = int x
 prettyF r (Add e1 e2)  = text "(" <> r e1 <> text "+" <> r e2 <> text ")"

-- Lambda

data Lambda e = Lam String e | FVar String | BVar Int deriving (Functor, Show)

parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

parseVar :: Elem Lambda fs -> Parser (Fix fs)
parseVar e = pure (In e . FVar) <*> parseVarName

parseLambda :: Elem Lambda fs -> Parser (Fix fs) -> Parser (Fix fs)
parseLambda e p = parseLam e p <|> parseVar e

parseLam :: Elem Lambda fs -> Parser (Fix fs) -> Parser (Fix fs)
parseLam e p = do
  char '\\'
  v <- parseVarName
  char '.'
  expr <- p
  return $ In e (Lam v expr)

instance Syntax Lambda where
 parseF = parseLambda
 prettyF r (Lam s e) = text ("\\" ++ s ++ ".") <> r e
 prettyF r (FVar s) = text s
 prettyF r (BVar i) = text "@" <> int i

-- Application

data App e = App e e deriving (Functor, Show)

parseApp :: Elem App fs -> Parser (Fix fs) -> Parser (Fix fs)
parseApp e p = chainlR (char ' ' >> p) App e p

instance Syntax App where
 parseF = parseApp
 prettyF r (App e1 e2) = text "[" <> r e1 <> text " " <> r e2 <> text "]"

-- Access

data Access e = AccPost e String | AccPre e String deriving (Functor, Show)

parseAcc :: Elem Access fs -> Parser (Fix fs) -> Parser (Fix fs)
parseAcc e p = chainlR (char '_' >> parseVarName) AccPost e p

instance Syntax Access where
 parseF = parseAcc
 prettyF r (AccPost e s) = r e <> text ("." ++ s)

-- TESTING

s :: Syntactic '[App, Access, Arith, Lambda]
s = CCons (CCons (CCons (CCons CVoid)))

run = runP s

test1 = run "(1+2)+3"
test2 = run "1+(2+3)"
test3 = run "x+y+(z+w)"
test4 = run "\\x.x+1 \\y.y"
test5 = run "(1+2)+3 (4+(6 (7 8)) 8+9+10)"
r = sequence_ [test1, test2, test3, test4, test5]
-}

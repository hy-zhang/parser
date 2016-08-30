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

module FullUntyped3 (TmRecord(..), TmFloat(..), TmString(..), TmLet(..)) where

import           Lib
import           Text.Parsec          hiding (runP)
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as Token
import           Text.PrettyPrint     hiding (char, space)
import           Data.List            (permutations)
import           GHC.TypeLits
import           Data.Proxy
import           Arith1
import           Untyped2


emptyTokenParser = Token.makeTokenParser emptyDef

-- Record

type Tag = String

data TmRecord e l where
  TmRecord :: [(Tag, e 0)] -> TmRecord e 0
  TmProj :: e 0 -> Tag -> TmRecord e 0

instance HFunctor TmRecord where
  hfmap f (TmRecord xs) = TmRecord $ map (mapSnd f) xs
  hfmap f (TmProj e t) = TmProj (f e) t

instance MyShow TmRecord where
  showMe (TmRecord xs) = "{" ++ foldl1 (\x y -> x ++ "," ++ y) (map f xs) ++ "}"
    where f = \(t, In _ e) -> t ++ "=" ++ showMe e
  showMe (TmProj (In _ e) t) = showMe e ++ "." ++ t

parseTmRecord :: NewParser TmRecord fs 0
parseTmRecord e ps = try $ choiceR e [parseProj, parseRec] -- can we avoid "try" (the case is left-recursive)?
  where
    parseRec = between (keyword "{") (keyword "}") ((In e . TmRecord) <$> parseFields)
    parseFields = parseField `sepBy` keyword ","
    parseField = do
      tag <- parseWord
      keyword "="
      field <- ps !!! (Proxy :: Proxy 0)
      return (tag, field)
    parseProj = chainlR (char '.' >> parseWord) TmProj e (ps !!! (Proxy :: Proxy 0))

instance Syntax TmRecord 0 where
  parseF _ = parseTmRecord

-- Float

data TmFloat e l where
  TmFloat :: Double -> TmFloat e 0
  TmTimesFloat :: e 0 -> e 0 -> TmFloat e 0

instance HFunctor TmFloat where
  hfmap _ (TmFloat d) = TmFloat d
  hfmap f (TmTimesFloat e1 e2) = TmTimesFloat (f e1) (f e2)

instance MyShow TmFloat where
  showMe (TmFloat d) = show d
  showMe (TmTimesFloat (In _ e1) (In _ e2)) = "timesfloat (" ++ showMe e1 ++ ") (" ++ showMe e2 ++ ")"

parseTmFloat :: NewParser TmFloat fs 0
parseTmFloat e ps = parseFloat <|> parseTimesFloat
  where
    parseFloat = (In e . TmFloat) <$> Token.float emptyTokenParser
    parseTimesFloat = do
      keyword "timesfloat"
      e1 <- ps !!! (Proxy :: Proxy 0)
      spaces
      e2 <- ps !!! (Proxy :: Proxy 0)
      return $ In e $ TmTimesFloat e1 e2

instance Syntax TmFloat 0 where
  keywords _ _ = ["timesfloat"]
  parseF _ = parseTmFloat

-- String

data TmString (e :: Nat -> *) l where
  TmString :: String -> TmString e 0

instance HFunctor TmString where
  hfmap _ (TmString s) = TmString s

instance MyShow TmString where
  showMe (TmString s) = show s

parseTmString :: NewParser TmString fs 0
parseTmString e _ = (In e . TmString) <$> Token.stringLiteral emptyTokenParser

instance Syntax TmString 0 where
  parseF _ = parseTmString

-- Let

data TmLet (e :: Nat -> *) l where
  TmLet :: String -> e 0 -> e 0 -> TmLet e 0

instance HFunctor TmLet where
  hfmap f (TmLet s e1 e2) = TmLet s (f e1) (f e2)

instance MyShow TmLet where
  showMe (TmLet s (In _ e1) (In _ e2)) = "let " ++ s ++ " = " ++ showMe e1 ++ " in " ++ showMe e2

parseTmLet :: NewParser TmLet fs 0
parseTmLet e ps = do
  keyword "let"
  x <- parseWord
  keyword "="
  expr <- ps !!! (Proxy :: Proxy 0)
  keyword "in"
  body <- ps !!! (Proxy :: Proxy 0)
  return $ In e (TmLet x expr body)

instance Syntax TmLet 0 where
  keywords _ _ = ["let", "in"]
  parseF _ = parseTmLet

-- s :: [Int] -> Syntactic '[TmBool, TmNat, TmArith, TmVar, TmLam, TmApp, TmRecord, TmFloat, TmString, TmLet] '[0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
-- s (x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : xs) = CCons x1 $ CCons x2 $ CCons x3 $ CCons x4 $ CCons x5 $ CCons x6 $ CCons x7 $ CCons x8 $ CCons x9 $ CCons x10 $ CVoid

-- r = testRun s 10 (Proxy :: Proxy 0)
-- r' = test s 10 (Proxy :: Proxy 0)

s :: [(Int, Bool)] -> Syntactic '[TmBool, TmNat, TmArith, TmVar, TmLam, TmApp, TmRecord, TmFloat, TmString, TmLet] '[0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
s (x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : xs) = CCons x1 $ CCons x2 $ CCons x3 $ CCons x4 $ CCons x5 $ CCons x6 $ CCons x7 $ CCons x8 $ CCons x9 $ CCons x10 $ CVoid

r1 = "if iszero (\\x.x y) then 1.0 else pred (succ {x=let s=(\\x.x x) in s,y=\\x.x x,z=\"true\"})"
r2 = "{x=succ \"let x=x in x\",y=pred (succ (iszero (\\x.x {x=1.1})))}"
r3 = "(\\x.x) ((\\x.{x=x,y=\\x.x x}.y) (timesfloat \"times\" (let y={x=1.0} in \"y\")))"

l = [(3, False), (4, False), (5, False), (1, False), (6, False), (7, True), (8, False), (2, False), (9, False), (10, False)]

r = map (show . testRun (s l) (Proxy :: Proxy 0)) input
input = ["x",
         "y'",
         "if x then false else x",
         "\\x.x",
         "(\\x.x)   \\x.x",
         "\\x.(x)",
         "\\x.(x x)",
         "\\x.x \\x.x",
         "2.0",
         "timesfloat 2.0 3.0",
         "timesfloat (timesfloat 2.0 3.0) (timesfloat 4.0 5.0)",
         "\"hello\"",
         "0",
         "succ (pred 0)",
         "iszero (pred (succ (succ 0)))",
         "let x=true in x",
         "{x=\\x.x, y=(\\x.x)(\\x.x)}",
         "{x=\\x.x, y=(\\x.x)(\\x.x)}.x",
         "{x=1.0}.t", r1, r2, r3]

output = ["\"x\"","\"y'\"","\"if (x) then (false) else (x)\"","\"[\\\\x. (x)]\"","\"([\\\\x. (x)] [\\\\x. (x)])\"","\"[\\\\x. (x)]\"","\"[\\\\x. ((x x))]\"","\"([\\\\x. (x)] [\\\\x. (x)])\"","\"2.0\"","\"timesfloat (2.0) (3.0)\"","\"timesfloat (timesfloat (2.0) (3.0)) (timesfloat (4.0) (5.0))\"","\"\\\"hello\\\"\"","\"0\"","\"succ (pred (0))\"","\"iszero (pred (succ (succ (0))))\"","\"let x = true in x\"","\"{x=[\\\\x. (x)],y=([\\\\x. (x)] [\\\\x. (x)])}\"","\"{x=[\\\\x. (x)],y=([\\\\x. (x)] [\\\\x. (x)])}.x\"","\"{x=1.0}.t\"","\"if (iszero (([\\\\x. (x)] y))) then (1.0) else (pred (succ ({x=let s = ([\\\\x. (x)] x) in s,y=([\\\\x. (x)] x),z=\\\"true\\\"})))\"","\"{x=succ (\\\"let x=x in x\\\"),y=pred (succ (iszero (([\\\\x. (x)] {x=1.1}))))}\"","\"([\\\\x. (x)] ([\\\\x. ({x=x,y=([\\\\x. (x)] x)})].y timesfloat (\\\"times\\\") (let y = {x=1.0} in \\\"y\\\")))\""]

-- l :: Int -> [Int]
-- l x = [6,8,10,2,12,14,16,4,18,x]

-- l' :: [Bool]
-- l' = [False,False,False,False,False,True,False,False,False]

-- f :: [Int] -> [Bool] -> [String] -> ([Bool], Int)
-- f xs ys inputs = res
  -- where
    -- len = length xs
    -- check = \bs -> \input -> testRun (s (zip xs bs)) (Proxy :: Proxy 0) input
    -- --bss = concat [gen i (len - i) [] | i <- [0..len]]
    -- bss = [l' ++ [True], l' ++ [False]]
    -- outputs = map (testRun (s (zip xs (repeat True))) (Proxy :: Proxy 0)) inputs
    -- record = [(bs, trues bs) | bs <- bss, and (zipWith (==) (map (check bs) inputs) outputs)]
    -- trues = foldl (\acc x -> if x == True then acc + 1 else acc) 0
    -- res = foldl1 (\a b -> if snd a < snd b then a else b) record

-- g = testRun (s (zip (l 20) (repeat True))) (Proxy :: Proxy 0)
-- runMe = sequence_ [putStrLn (show i ++ ": " ++ (show $ f (l i) l' [r1,r2,r3])) | i <- [19]]

-- gen :: Int -> Int -> [Bool] -> [[Bool]]
-- gen 0 0 l = [l]
-- gen a 0 l = gen (a - 1) 0 (True : l)
-- gen 0 b l = gen 0 (b - 1) (False : l)
-- gen a b l = gen (a - 1) b (True : l) ++ gen a (b - 1) (False : l)

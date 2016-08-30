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

module Untyped2 (TmVar(..), TmLam(..), TmApp(..)) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)
import           GHC.TypeLits
import           Data.Proxy
import           Arith1

-- Var

type VarId = String

data TmVar (e :: Nat -> *) l where
  TmVar :: VarId -> TmVar e 0

instance HFunctor TmVar where
  hfmap _ (TmVar s) = TmVar s

instance MyShow TmVar where
  showMe (TmVar s) = s

parseTmVar :: NewParser TmVar fs 0
parseTmVar e _ = (In e . TmVar) <$> parseWord

instance Syntax TmVar 0 where
  parseF _ = parseTmVar

-- Lam

data TmLam e l where
  TmLam :: VarId -> e 0 -> TmLam e 0

instance HFunctor TmLam where
  hfmap f (TmLam s e) = TmLam s (f e)

instance MyShow TmLam where
  showMe (TmLam s (In _ e)) = "[\\" ++ s ++ ". (" ++ showMe e ++ ")]"

parseTmLam :: NewParser TmLam fs 0
parseTmLam e p = do
  keyword "\\"
  x <- parseWord
  keyword "."
  body <- p !!! (Proxy :: Proxy 0)
  return $ In e (TmLam x body)

instance Syntax TmLam 0 where
  parseF _ = parseTmLam

-- App

data TmApp e l where
  TmApp :: e 0 -> e 0 -> TmApp e 0

instance HFunctor TmApp where
  hfmap f (TmApp e1 e2) = TmApp (f e1) (f e2)

instance MyShow TmApp where
  showMe (TmApp (In _ e1) (In _ e2)) = "(" ++ showMe e1 ++ " " ++ showMe e2 ++ ")"

parseTmApp :: NewParser TmApp fs 0
parseTmApp e p = chainlR (spaces >> (p !!! (Proxy :: Proxy 0))) TmApp e (p !!! (Proxy :: Proxy 0))

instance Syntax TmApp 0 where
  parseF _ = parseTmApp

-- s :: [Int] -> Syntactic '[TmBool, TmNat, TmArith, TmVar, TmLam, TmApp] '[0, 0, 0, 0, 0, 0]
-- s (x1 : x2 : x3 : x4 : x5 : x6 : xs) = CCons 1 $ CCons 2 $ CCons 3 $ CCons x1 $ CCons x2 $ CCons x3 $ CVoid

-- r = testRun s 6 (Proxy :: Proxy 0)
-- r' = test (s [4,2,5,1,6,3]) (Proxy :: Proxy 0)
  -- [("x", "x"),
   -- ("\\x.x", "\\x. (x)"),
   -- ("(\\x.x) (\\x.x x)", "(\\x. (x) \\x. ((x x)))")]

-- s :: [Int] -> Syntactic '[TmBool, TmNat, TmArith, TmVar, TmLam, TmApp] '[0, 0, 0, 0, 0, 0]
-- s (x1 : x2 : x3 : x4 : x5 : x6 : xs) = CCons x1 $ CCons x2 $ CCons x3 $ CCons x4 $ CCons x5 $ CCons x6 $ CVoid

-- r1 = "if iszero (\\x.x y) then true else pred (succ 0)"
-- r2 = "\\y.if (if (\\x.y) then y else ((succ z) (\\x.succ 0))) then true else (succ 0)"
-- r3 = "succ (pred (succ (iszero ((\\x.x) (\\x.x x)))))"
-- r = testRun (s [4,6,8,2,10,12]) (Proxy :: Proxy 0)
-- input = ["x",
         -- "y'",
         -- "\\x.x",
         -- "(\\x.x)   \\x.x",
         -- "\\x.(x)",
         -- "\\x.(x x)",
         -- "\\x.x \\x.x"]


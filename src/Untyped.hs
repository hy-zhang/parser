{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Untyped (TmVar(..), TmLam(..), TmApp(..)) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)

-- Var

type VarId = String

data TmVar e = TmVar VarId deriving (Functor, Show)

parseTmVar :: NewParser TmVar fs
parseTmVar e _ = (In e . TmVar) <$> parseWord

instance Syntax TmVar where
  parseF                    = parseTmVar
  prettyF _ (TmVar v)       = text v

-- Lam

data TmLam e = TmLam VarId e deriving (Functor, Show)

parseTmLam :: NewParser TmLam fs
parseTmLam e p = do
  _ <- char '\\'
  x <- parseWord
  _ <- char '.'
  body <- p
  return $ In e (TmLam x body)

instance Syntax TmLam where
  parseF                   = parseTmLam
  prettyF r (TmLam x body) = text "\\" <> text x <> text "." <> r body

-- App

data TmApp e = TmApp e e deriving (Functor, Show)

parseTmApp :: NewParser TmApp fs
parseTmApp e p = chainlR (spaces >> p) TmApp e p

instance Syntax TmApp where
  parseF = parseTmApp
  prettyF r (TmApp e1 e2)  = parens (r e1 <+> r e2)

-- Test

s :: Syntactic '[TmApp, TmLam, TmVar]
s = crep

test :: IO ()
test = mapM_ (runP s) [
  "x",
  "y'",
  "\\x.x",
  "(\\x.x)   \\x.x",
  "\\x.(x)",
  "\\x.(x x)",
  "\\x.x \\x.x"]

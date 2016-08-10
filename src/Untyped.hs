{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Untyped where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)


parseWord = many1 (letter <|> char '\'')

-- Var

type VarId = String

data TmVar e = TmVar VarId deriving (Functor, Show)

parseTmVar :: NewParser TmVar fs
parseTmVar e _ = (In e . TmVar) <$> parseWord

instance Syntax TmVar where
  parseF                    = parseTmVar
  prettyF _ (TmVar v)       = text $ show v

-- Lam & App

data TmLamApp e = TmLam VarId e | TmApp e e deriving (Functor, Show)

parseTmLamApp :: NewParser TmLamApp fs
parseTmLamApp e p = parseLam <|> parseApp
  where
    parseLam = do
      _ <- char '\\'
      x <- parseWord
      _ <- char '.'
      body <- p
      return $ In e (TmLam x body)
    parseApp = do
      e1 <- p
      spaces
      e2 <- p
      return $ In e (TmApp e1 e2)

instance Syntax TmLamApp where
  parseF                   = parseTmLamApp
  prettyF r (TmLam x body) = text "\\" <> text (show x) <> text "." <> r body
  prettyF r (TmApp e1 e2)  = parens (r e1) <+> parens (r e2)

-- Test

s :: Syntactic '[TmVar, TmLamApp]
s = CCons (CCons CVoid)

test :: IO ()
test = mapM_ (runP s) [
  "x",
  "y'",
  "\\x.x",
  -- Wrong
  "\\x.x \\x.x"]

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullPoly ( TyAll(..)
                , TySome(..)
                , TmTAbs(..)
                , TmTApp(..)
                , TmPack(..)
                , TmUnpack(..)
                ) where

import           Lib
import           Text.Parsec          hiding (runP)
import           Text.PrettyPrint hiding (char, space)

-- TyAll

data TyAll e = TyAll String e deriving (Functor, Show)

parseTyAll :: NewParser TyAll fs
parseTyAll e p = do
  keyword "All"
  x <- parseWordUpper
  keyword "."
  t <- p
  return $ In e (TyAll x t)

instance Syntax TyAll where
  keywords _            = ["All"]
  parseF                = parseTyAll
  prettyF r (TyAll x t) = "All" <+> text x <> "." <> r t

-- TySome

data TySome e = TySome String e deriving (Functor, Show)

parseTySome :: NewParser TySome fs
parseTySome e p = between (keyword "{") (keyword "}") $ do
  keyword "Some"
  x <- parseWordUpper
  keyword ","
  t <- p
  return $ In e (TySome x t)

instance Syntax TySome where
  keywords _             = ["Some"]
  parseF                 = parseTySome
  prettyF r (TySome x t) = braces $ "Some" <+> text x <> "," <+> r t

-- TmTAbs

data TmTAbs e = TmTAbs String e deriving (Functor, Show)

parseTmTAbs :: NewParser TmTAbs fs
parseTmTAbs e p = do
  keyword "\\"
  x <- parseWordUpper
  keyword "."
  body <- p
  return $ In e (TmTAbs x body)

instance Syntax TmTAbs where
  parseF = parseTmTAbs
  prettyF r (TmTAbs x e) = "\\" <> text x <> "." <> r e

-- TmTApp

data TmTApp e = TmTApp e e deriving (Functor, Show)

parseTmTApp :: NewParser TmTApp fs
parseTmTApp e p = chainlR p' TmTApp e p
  where
    p' = between (keyword "[") (keyword "]") p

instance Syntax TmTApp where
  parseF = parseTmTApp
  prettyF r (TmTApp e1 e2) = parens $ r e1 <+> brackets (r e2)

-- TmPack

data TmPack e = TmPack e e e deriving (Functor, Show)

parseTmPack :: NewParser TmPack fs
parseTmPack e p = do
  keyword "{"
  keyword "*"
  t1 <- p
  keyword ","
  e1 <- p
  keyword "}"
  keyword "as"
  t2 <- p
  return $ In e (TmPack t1 e1 t2)

instance Syntax TmPack where
  keywords _ = ["as"]
  parseF = parseTmPack
  prettyF r (TmPack t1 e t2) = braces ("*" <> r t1 <> "," <+> r e) <+> "as" <+> r t2

-- TmUnpack

data TmUnpack e = TmUnpack String String e e deriving (Functor, Show)

parseTmUnPack :: NewParser TmUnpack fs
parseTmUnPack e p = do
  keyword "let"
  keyword "{"
  t <- parseWordUpper
  keyword ","
  x <- parseWord
  keyword "}"
  keyword "="
  e1 <- p
  keyword "in"
  e2 <- p
  return $ In e (TmUnpack t x e1 e2)

instance Syntax TmUnpack where
  keywords _ = ["let", "in"]
  parseF = parseTmUnPack
  prettyF r (TmUnpack t x e1 e2) = "let" <+> braces (text t <> "," <> text x) <+> "=" <+> r e1 <+> "in" <+> r e2

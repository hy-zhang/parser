{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullSimple (TmUnit(..),
                   TyUnit(..),
                   TmAscribe(..),
                   TmTag(..),
                   TyVariant(..),
                   TmFix(..),
                   TmCase(..),
                   TyVar(..),
                   TyString) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)

-- TmUnit

data TmUnit e = TmUnit deriving (Functor, Show)

parseTmUnit :: NewParser TmUnit fs
parseTmUnit e _ = keyword "unit" >> return (In e TmUnit)

instance Syntax TmUnit where
  keywords _       = ["unit"]
  parseF           = parseTmUnit
  prettyF _ TmUnit = "unit"

-- TyUnit

data TyUnit e = TyUnit deriving (Functor, Show)

parseTyUnit :: NewParser TyUnit fs
parseTyUnit e _ = keyword "Unit" >> return (In e TyUnit)

instance Syntax TyUnit where
  keywords _       = ["Unit"]
  parseF           = parseTyUnit
  prettyF _ TyUnit = "Unit"

-- TmAscribe

data TmAscribe e = TmAscribe e e deriving (Functor, Show)

parseTmAscribe :: NewParser TmAscribe fs
parseTmAscribe e p = chainlR (keyword "as" >> p) TmAscribe e p

instance Syntax TmAscribe where
  keywords _                = ["as"]
  parseF                    = parseTmAscribe
  prettyF r (TmAscribe e t) = parens $ r e <+> "as" <+> r t

-- TmTag

data TmTag e = TmTag String e e deriving (Functor, Show)

parseTmTag :: NewParser TmTag fs
parseTmTag e p = do
  keyword "<"
  l <- parseWord
  keyword "="
  expr <- p
  keyword ">"
  keyword "as"
  typ <- p
  return $ In e (TmTag l expr typ)

instance Syntax TmTag where
  keywords _                = ["as"]
  parseF                    = parseTmTag
  prettyF r (TmTag l e t)   = parens $ "<" <> text l <> "=" <> r e <> ">" <+> "as" <+> r t

-- TyVariant

data TyVariant e = TyVariant [(String, e)] deriving (Functor, Show)

parseTyVariant :: NewParser TyVariant fs
parseTyVariant e p = do
  keyword "<"
  fs <- pf
  keyword ">"
  return $ In e (TyVariant fs)
  where
    pf = p1 `sepBy1` keyword ","
    p1 = do
      l <- parseWord
      keyword ":"
      t <- p
      return (l, t)

instance Syntax TyVariant where
  parseF                   = parseTyVariant
  prettyF r (TyVariant fs) =
    let gs = map (\(l, t) -> text l <> colon <> r t) fs in
      "<" <> foldl1 (\a b -> a <> comma <+> b) gs <> ">"

-- TmFix

data TmFix e = TmFix e deriving (Functor, Show)

parseTmFix :: NewParser TmFix fs
parseTmFix e p = do
  keyword "fix"
  expr <- p
  return $ In e (TmFix expr)

instance Syntax TmFix where
  keywords _ = ["fix"]
  parseF = parseTmFix
  prettyF r (TmFix e) = "fix" <+> parens (r e)

-- TmCase

data TmCase e = TmCase e [(String, String, e)] deriving (Functor, Show)

parseTmCase :: NewParser TmCase fs
parseTmCase e p = do
  keyword "case"
  expr <- p
  keyword "of"
  cs <- pCases
  return $ In e (TmCase expr cs)
  where
    pCases = pCase `sepBy1` keyword "|"
    pCase = do
      keyword "<"
      l <- parseWord
      keyword "="
      x <- parseWord
      keyword ">"
      keyword "=>"
      r <- p
      return (l, x, r)

instance Syntax TmCase where
  keywords _ = ["case", "of"]
  parseF = parseTmCase
  prettyF r (TmCase expr cs) =
    let gs = map (\(l, x, e) -> "<" <> text l <> "=" <> text x <> ">" <+> "=>" <+> r e) cs in
      "case" <+> r expr <+> "of" <+> foldl1 (\x y -> x <+> "|" <+> y) gs

-- TyVar

data TyVar e = TyVar String deriving (Functor, Show)

parseTyVar :: NewParser TyVar fs
parseTyVar e _ = do
  w <- parseWordUpper
  return $ In e (TyVar w)

instance Syntax TyVar where
  parseF = parseTyVar
  prettyF _ (TyVar v) = text v

-- TyString

data TyString e = TyString deriving (Functor, Show)

parseTyString :: NewParser TyString fs
parseTyString e _ = keyword "String" >> return (In e TyString)

instance Syntax TyString where
  keywords _ = ["String"]
  parseF = parseTyString
  prettyF _ TyString = "String"

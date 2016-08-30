{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullOmega ( KnStar(..)
                 , KnArr(..)
                 , TyAll2(..)
                 , TySome2(..)
                 , TmTAbs2(..)) where

import           Lib
import           Text.Parsec          hiding (runP)
import           Text.PrettyPrint hiding (char, space)

-- KnStar

data KnStar e = KnStar deriving (Functor, Show)

parseKnStar :: NewParser KnStar fs
parseKnStar e _ = keyword "Star" >> return (In e KnStar)

instance Syntax KnStar where
  keywords _       = ["Star"]
  parseF           = parseKnStar
  prettyF _ KnStar = "Star"

-- KnArr

data KnArr e = KnArr e e deriving (Functor, Show)

parseKnArr :: NewParser KnArr fs
parseKnArr e p = chainlR (keyword "=>" >> p) KnArr e p

instance Syntax KnArr where
  parseF = parseKnArr
  prettyF r (KnArr e1 e2) = r e1 <> "=>" <> r e2

-- TyAll2

data TyAll2 e = TyAll2 String e e deriving (Functor, Show)

parseTyAll2 :: NewParser TyAll2 fs
parseTyAll2 e p = do
  keyword "All"
  x <- parseWordUpper
  keyword ":"
  k <- p
  keyword "."
  t <- p
  return $ In e (TyAll2 x k t)

instance Syntax TyAll2 where
  keywords _               = ["All"]
  parseF                   = parseTyAll2
  prettyF r (TyAll2 x k t) = "All" <+> text x <> colon <> r k <> "." <> r t

-- TySome2

data TySome2 e = TySome2 String e e deriving (Functor, Show)

parseTySome2 :: NewParser TySome2 fs
parseTySome2 e p = between (keyword "{") (keyword "}") $ do
  keyword "Some"
  x <- parseWordUpper
  keyword ":"
  k <- p
  keyword ","
  t <- p
  return $ In e (TySome2 x k t)

instance Syntax TySome2 where
  keywords _                = ["Some"]
  parseF                    = parseTySome2
  prettyF r (TySome2 x k t) = braces $ "Some" <+> text x <> colon <> r k <> "," <+> r t

-- TyAbs & TyApp cannot be distinguished in one fixpoint setting

-- TmTAbs

data TmTAbs2 e = TmTAbs2 String e e deriving (Functor, Show)

parseTmTAbs2 :: NewParser TmTAbs2 fs
parseTmTAbs2 e p = do
  keyword "\\"
  x <- parseWordUpper
  keyword ":"
  k <- p
  keyword "."
  body <- p
  return $ In e (TmTAbs2 x k body)

instance Syntax TmTAbs2 where
  parseF = parseTmTAbs2
  prettyF r (TmTAbs2 x k e) = "\\" <> text x <> colon <> r k <> "." <> r e

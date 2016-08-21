{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module SimpleBool (TyArr(..), TmLam2(..)) where

import           Lib
import           Text.PrettyPrint hiding (char, space)

-- TyArr

data TyArr e = TyArr e e deriving (Functor, Show)

parseTyArr :: NewParser TyArr fs
parseTyArr e p = chainlR (keyword "->" >> p) TyArr e p

instance Syntax TyArr where
  parseF                    = parseTyArr
  prettyF r (TyArr t1 t2)   = r t1 <> text "->" <> r t2


-- New lambda

data TmLam2 e = TmLam2 String e e deriving (Functor, Show)

parseTmLam2 :: NewParser TmLam2 fs
parseTmLam2 e p = do
  keyword "\\"
  x <- parseWord
  keyword ":"
  t <- p
  keyword "."
  body <- p
  return $ In e (TmLam2 x t body)

instance Syntax TmLam2 where
  parseF = parseTmLam2
  prettyF r (TmLam2 x t e) = text "\\" <> text x <> colon <> r t <> text "." <> r e

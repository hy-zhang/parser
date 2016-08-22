{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RcdSubBot (TyRecord(..)) where

import           Lib
import           Text.Parsec
import           Text.PrettyPrint hiding (char, space)


-- TyRecord

data TyRecord e = TyRecord [(String, e)] deriving (Functor, Show)

parseTyRecord :: NewParser TyRecord fs
parseTyRecord e p = between (keyword "{") (keyword "}") ((In e . TyRecord) <$> parseFields)
  where
    parseFields = parseField `sepBy` keyword ","
    parseField = do
      tag <- parseWord
      keyword ":"
      field <- p
      return (tag, field)

instance Syntax TyRecord where
  parseF = parseTyRecord
  prettyF r (TyRecord fields) =
    braces $ hsep $ punctuate (text ",") (map (\(t, f) -> text t <> text ":" <> r f) fields)

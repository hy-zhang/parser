{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullUntyped (TmRecord(..), TmFloat(..), TmString(..), TmLet(..)) where

import           Lib
import           Text.Parsec          hiding (runP)
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as Token
import           Text.PrettyPrint     hiding (char, space)


emptyTokenParser = Token.makeTokenParser emptyDef

-- Record

type Tag = String

data TmRecord e = TmRecord [(Tag, e)] | TmProj e Tag deriving (Functor, Show)

parseTmRecord :: NewParser TmRecord fs
parseTmRecord e p = choiceR e [parseProj, parseRec]
  where
    parseRec = between (keyword "{") (keyword "}") ((In e . TmRecord) <$> parseFields)
    parseFields = parseField `sepBy` keyword ","
    parseField = do
      tag <- parseWord
      keyword "="
      field <- p
      return (tag, field)
    parseProj = chainlR (char '.' >> parseWord) TmProj e p

instance Syntax TmRecord where
  parseF = parseTmRecord
  prettyF r (TmRecord fields) =
    braces $ hsep $ punctuate (text ",") (map (\(t, f) -> text t <> text "=" <> r f) fields)
  prettyF r (TmProj e tag) =
    r e <> text "." <> text tag

-- Float

data TmFloat e = TmFloat Double | TmTimesFloat e e deriving (Functor, Show)

parseTmFloat :: NewParser TmFloat fs
parseTmFloat e p = parseFloat <|> parseTimesFloat
  where
    parseFloat = (In e . TmFloat) <$> Token.float emptyTokenParser
    parseTimesFloat = do
      keyword "timesfloat"
      e1 <- p
      spaces
      e2 <- p
      return $ In e $ TmTimesFloat e1 e2

instance Syntax TmFloat where
  keywords _ = ["timesfloat"]
  parseF = parseTmFloat
  prettyF _ (TmFloat val) = text $ show val
  prettyF r (TmTimesFloat e1 e2) = text "timesfloat" <+> parens (r e1) <+> parens (r e2)

-- String

data TmString e = TmString String deriving (Functor, Show)

parseTmString :: NewParser TmString fs
parseTmString e _ = (In e . TmString) <$> Token.stringLiteral emptyTokenParser

instance Syntax TmString where
  parseF = parseTmString
  prettyF _ (TmString str) = text $ show str

-- Let

data TmLet e = TmLet String e e deriving (Functor, Show)

parseTmLet :: NewParser TmLet fs
parseTmLet e p = do
  keyword "let"
  x <- parseWord
  keyword "="
  expr <- p
  keyword "in"
  body <- p
  return $ In e (TmLet x expr body)

instance Syntax TmLet where
  keywords _ = ["let", "in"]
  parseF = parseTmLet
  prettyF r (TmLet x e body) = text "let" <+> text x <+> text "=" <+> r e <+> text "in" <+> r body

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullEquiRec (TyRec(..)) where

import           Lib
import           Text.PrettyPrint hiding (char, space)


-- TyRec

data TyRec e = TyRec String e deriving (Functor, Show)

parseTyRec :: NewParser TyRec fs
parseTyRec e p = do
  keyword "Rec"
  w <- parseWord
  keyword "."
  t <- p
  return $ In e (TyRec w t)

instance Syntax TyRec where
  keywords _ = ["Rec"]
  parseF = parseTyRec
  prettyF r (TyRec t e) = "Rec" <+> text t <> "." <> r e

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullRef (TyTop(..), TyBot(..), TyRef(..), TmRef(..),
                TmDeref(..), TmAssign(..), TySource(..), TySink(..)) where

import           Lib
import           Text.PrettyPrint hiding (char, space)

-- TyTop

data TyTop e = TyTop deriving (Functor, Show)

parseTyTop :: NewParser TyTop fs
parseTyTop e _ = keyword "Top" >> return (In e TyTop)

instance Syntax TyTop where
  keywords _      = ["Top"]
  parseF          = parseTyTop
  prettyF _ TyTop = "Top"

-- TyBot

data TyBot e = TyBot deriving (Functor, Show)

parseTyBot :: NewParser TyBot fs
parseTyBot e _ = keyword "Bot" >> return (In e TyBot)

instance Syntax TyBot where
  keywords _      = ["Bot"]
  parseF          = parseTyBot
  prettyF _ TyBot = "Bot"

-- TyRef

data TyRef e = TyRef e deriving (Functor, Show)

parseTyRef :: NewParser TyRef fs
parseTyRef e p = do
  keyword "Ref"
  t <- p
  return $ In e (TyRef t)

instance Syntax TyRef where
  keywords _          = ["Ref"]
  parseF              = parseTyRef
  prettyF r (TyRef e) = "Ref" <+> r e

-- TmRef

data TmRef e = TmRef e deriving (Functor, Show)

parseTmRef :: NewParser TmRef fs
parseTmRef e p = do
  keyword "ref"
  t <- p
  return $ In e (TmRef t)

instance Syntax TmRef where
  keywords _          = ["ref"]
  parseF              = parseTmRef
  prettyF r (TmRef e) = "ref" <+> r e

-- TmDeref

data TmDeref e = TmDeref e deriving (Functor, Show)

parseTmDeref :: NewParser TmDeref fs
parseTmDeref e p = do
  keyword "!"
  t <- p
  return $ In e (TmDeref t)

instance Syntax TmDeref where
  parseF              = parseTmDeref
  prettyF r (TmDeref e) = parens $ "!" <> r e

-- TmAssign

data TmAssign e = TmAssign e e deriving (Functor, Show)

parseTmAssign :: NewParser TmAssign fs
parseTmAssign e p = chainlR (keyword ":=" >> p) TmAssign e p

instance Syntax TmAssign where
  parseF = parseTmAssign
  prettyF p (TmAssign l r) = p l <+> ":=" <+> p r

-- TySource

data TySource e = TySource e deriving (Functor, Show)

parseTySource :: NewParser TySource fs
parseTySource e p = do
  keyword "Source"
  t <- p
  return $ In e (TySource t)

instance Syntax TySource where
  keywords _             = ["Source"]
  parseF                 = parseTySource
  prettyF r (TySource e) = "Source" <+> r e

-- TySink

data TySink e = TySink e deriving (Functor, Show)

parseTySink :: NewParser TySink fs
parseTySink e p = do
  keyword "Sink"
  t <- p
  return $ In e (TySink t)

instance Syntax TySink where
  keywords _             = ["Sink"]
  parseF                 = parseTySink
  prettyF r (TySink e)   = "Sink" <+> r e

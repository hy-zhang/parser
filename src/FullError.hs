{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullError (TmError(..), TmTry(..)) where

import           Lib
import           Text.PrettyPrint hiding (char, space)


-- TmError

data TmError e = TmError deriving (Functor, Show)

parseTmError :: NewParser TmError fs
parseTmError e _ = keyword "error" >> return (In e TmError)

instance Syntax TmError where
  keywords _ = ["error"]
  parseF = parseTmError
  prettyF _ TmError = "error"


-- TmTry

data TmTry e = TmTry e e deriving (Functor, Show)

parseTmTry :: NewParser TmTry fs
parseTmTry e p = do
  keyword "try"
  e1 <- p
  keyword "with"
  e2 <- p
  return $ In e (TmTry e1 e2)

instance Syntax TmTry where
  keywords _ = ["try", "with"]
  parseF = parseTmTry
  prettyF r (TmTry e1 e2) = "try" <+> r e1 <+> "with" <+> r e2

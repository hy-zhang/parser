{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TyArith (TyBool(..), TyNat(..)) where

import           Lib
import           Text.PrettyPrint hiding (char, space)

-- TyBool

data TyBool e = TyBool deriving (Functor, Show)

parseTyBool :: NewParser TyBool fs
parseTyBool e _ = keyword "Bool" >> return (In e TyBool)

instance Syntax TyBool where
  keywords _                = ["Bool"]
  parseF                    = parseTyBool
  prettyF _ TyBool          = text "Bool"

-- TyNat

data TyNat e = TyNat deriving (Functor, Show)

parseTyNat :: NewParser TyNat fs
parseTyNat e _ = keyword "Nat" >> return (In e TyNat)

instance Syntax TyNat where
  keywords _               = ["Nat"]
  parseF                   = parseTyNat
  prettyF _ TyNat          = text "Nat"

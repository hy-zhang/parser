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

import           Arith

-- TyBool

data TyBool e = TyBool deriving (Functor, Show)

parseTyBool :: NewParser TyBool fs
parseTyBool e _ = keyword "Bool" >> return (In e TyBool)

instance Syntax TyBool where
  parseF                    = parseTyBool
  prettyF _ TyBool          = text "Bool"

-- TyNat

data TyNat e = TyNat deriving (Functor, Show)

parseTyNat :: NewParser TyNat fs
parseTyNat e _ = keyword "Nat" >> return (In e TyNat)

instance Syntax TyNat where
  parseF                   = parseTyNat
  prettyF _ TyNat          = text "Nat"

-- Test

s :: Syntactic '[TmBool, TmNat, TmArith, TyNat, TyBool]
s = crep

test :: IO ()
test = mapM_ (runP s) [
  "true",
  "if false then true else false",
  "0",
  "succ (pred 0)",
  "iszero (pred (succ (succ 0)))",
  "Nat",
  "Bool"]

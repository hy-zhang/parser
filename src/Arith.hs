{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Arith (TmBool(..), TmNat(..), TmArith(..)) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, parens, space)

-- TmBool

data TmBool e = TmTrue | TmFalse | TmIf e e e deriving (Functor, Show)

parseTmBool :: NewParser TmBool fs
parseTmBool e p =
  (string "true" >> pure (In e TmTrue)) <|>
  (string "false" >> pure (In e TmFalse)) <|>
  do { try (keyword "if"); e1 <- p;
       keyword "then"; e2 <- p;
       keyword "else"; e3 <- p;
       return $ In e (TmIf e1 e2 e3)}

instance Syntax TmBool where
  parseF                    = parseTmBool
  prettyF _ TmTrue          = text "true"
  prettyF _ TmFalse         = text "false"
  prettyF r (TmIf e1 e2 e3) = text "(if " <> r e1 <> text " then " <> r e2 <> text " else " <> r e3 <> text ")"

-- TmNat

data TmNat e = TmZero | TmSucc e | TmPred e deriving (Functor, Show)

parseTmNat :: NewParser TmNat fs
parseTmNat e p =
  (char '0' >> pure (In e TmZero)) <|>
  (keyword "succ" >> (pure (In e . TmSucc) <*> p)) <|>
  (keyword "pred" >> (pure (In e . TmPred) <*> p))

instance Syntax TmNat where
  parseF               = parseTmNat
  prettyF _ TmZero     = text "0"
  prettyF r (TmSucc e) = text "succ (" <> r e <> text ")"
  prettyF r (TmPred e) = text "pred (" <> r e <> text ")"

-- TmArith

data TmArith e = TmIsZero e deriving (Functor, Show)

parseTmArith :: NewParser TmArith fs
parseTmArith e p = try (keyword "iszero") >> (pure (In e . TmIsZero) <*> p)

instance Syntax TmArith where
  parseF                 = parseTmArith
  prettyF r (TmIsZero e) = text "iszero (" <> r e <> text ")"

-- Test

s :: Syntactic '[TmBool, TmNat, TmArith]
s = CCons (CCons (CCons CVoid))

test :: IO ()
test = mapM_ (runP s) [
  "true",
  "if false then true else false",
  "0",
  "succ (pred 0)",
  "iszero (pred (succ (succ 0)))"]

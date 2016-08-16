{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Arith (TmBool(..), TmNat(..), TmArith(..), test) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)

-- TmBool

data TmBool e = TmTrue | TmFalse | TmIf e e e deriving (Functor, Show)

parseTmBool :: NewParser TmBool fs
parseTmBool e p =
  (reserved "true" >> pure (In e TmTrue)) <|>
  (reserved "false" >> pure (In e TmFalse)) <|>
  do { reserved "if"; e1 <- p;
       reserved "then"; e2 <- p;
       reserved "else"; e3 <- p;
       return $ In e (TmIf e1 e2 e3)}

instance Syntax TmBool where
  keywords _                = ["true", "false", "if", "then", "else"]
  parseF                    = parseTmBool
  prettyF _ TmTrue          = text "true"
  prettyF _ TmFalse         = text "false"
  prettyF r (TmIf e1 e2 e3) = parens $ text "if " <> r e1 <> text " then " <> r e2 <> text " else " <> r e3

-- TmNat

data TmNat e = TmZero | TmSucc e | TmPred e deriving (Functor, Show)

parseTmNat :: NewParser TmNat fs
parseTmNat e p =
  (reserved "0" >> pure (In e TmZero)) <|>
  (reserved "succ" >> (In e . TmSucc) <$> p) <|>
  (reserved "pred" >> (In e . TmPred) <$> p)

instance Syntax TmNat where
  keywords _           = ["0", "succ", "pred"]
  parseF               = parseTmNat
  prettyF _ TmZero     = text "0"
  prettyF r (TmSucc e) = text "succ" <+> parens (r e)
  prettyF r (TmPred e) = text "pred" <+> parens (r e)

-- TmArith

data TmArith e = TmIsZero e deriving (Functor, Show)

parseTmArith :: NewParser TmArith fs
parseTmArith e p = reserved "iszero" >> (In e . TmIsZero <$> p)

instance Syntax TmArith where
  keywords _             = ["iszero"]
  parseF                 = parseTmArith
  prettyF r (TmIsZero e) = text "iszero" <+> parens (r e)

-- Test

s :: Syntactic '[TmBool, TmNat, TmArith]
s = crep

test :: IO ()
test = mapM_ (runP s) [
  "true",
  "if false then true else false",
  "0",
  "succ (pred 0)",
  "iszero (pred (succ (succ 0)))"]

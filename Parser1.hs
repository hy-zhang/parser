{-# OPTIONS -XGADTs -XDataKinds -XKindSignatures -XTypeOperators -XMultiParamTypeClasses -XFlexibleInstances -XDeriveFunctor -XFlexibleContexts -XScopedTypeVariables -XOverlappingInstances -XConstraintKinds  #-}

module Parser1 where

import Impl
import Text.Parsec hiding (Parser, runP)
import Text.Parsec.String hiding (Parser)
import Text.PrettyPrint hiding (char, space, parens)

-- TmBool

data TmBool e = TmTrue | TmFalse | TmIf e e e deriving (Functor, Show)

parseTmBool :: NewParser TmBool fs
parseTmBool e p =
  (string "true" >> pure (In e TmTrue)) <|>
  (string "false" >> pure (In e TmFalse)) <|>
  do { try (keywordS "if"); e1 <- p;
       keyword "then"; e2 <- p;
       keyword "else"; e3 <- p;
       return $ In e (TmIf e1 e2 e3)}

instance Syntax TmBool where
  parseF                    = parseTmBool
  prettyF r TmTrue          = text "true"
  prettyF r TmFalse         = text "false"
  prettyF r (TmIf e1 e2 e3) = text "(if " <> r e1 <> text " then " <> r e2 <> text " else " <> r                            e3 <> text ")"

-- TmNat

data TmNat e = TmZero | TmSucc e | TmPred e deriving (Functor, Show)

parseTmNat :: NewParser TmNat fs
parseTmNat e p = 
  (char '0' >> pure (In e TmZero)) <|>
  (keywordS "succ" >> (pure (In e . TmSucc) <*> p)) <|>
  (keywordS "pred" >> (pure (In e . TmPred) <*> p))

instance Syntax TmNat where
  parseF               = parseTmNat
  prettyF r TmZero     = text "0"
  prettyF r (TmSucc e) = text "succ (" <> r e <> text ")"
  prettyF r (TmPred e) = text "pred (" <> r e <> text ")"

-- TmArith

data TmArith e = TmIsZero e deriving (Functor, Show)

parseTmArith :: NewParser TmArith fs
parseTmArith e p = try (keywordS "iszero") >> (pure (In e . TmIsZero) <*> p)

instance Syntax TmArith where
  parseF                 = parseTmArith
  prettyF r (TmIsZero e) = text "iszero (" <> r e <> text ")"

-- Test

s :: Syntactic '[TmBool, TmNat, TmArith]
s = CCons (CCons (CCons CVoid))

run = runP s
r = sequence_ . map run $ [
  "true",
  "if false then true else false",
  "0",
  "succ (pred 0)",
  "iszero (pred (succ (succ 0)))"] 
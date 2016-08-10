{-# OPTIONS -XGADTs -XDataKinds -XKindSignatures -XTypeOperators -XMultiParamTypeClasses -XFlexibleInstances -XDeriveFunctor -XFlexibleContexts -XScopedTypeVariables -XOverlappingInstances -XConstraintKinds  #-}

module Parser1 where

import Impl
import Text.Parsec hiding (Parser, runP)
import Text.Parsec.String hiding (Parser)
import Text.PrettyPrint hiding (char, space, parens)

keywordS s = spaces >> string s >> space >> spaces
keyword  s = space >> keywordS s

-- TmBool

data TmBool e = TmTrue | TmFalse | TmIf e e e deriving (Functor, Show)

parseTmBool :: NewParser TmBool fs
parseTmBool e p =
  (string "true" >> pure (In e TmTrue)) <|>
  (string "false" >> pure (In e TmFalse)) <|>
  do { keywordS "if"; e1 <- p;
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
parseTmNat = undefined

instance Syntax TmNat where
  parseF = parseTmNat

-- TmArith

data TmArith e = TmIsZero e deriving (Functor, Show)

parseTmArith :: NewParser TmArith fs
parseTmArith = undefined

instance Syntax TmArith where
  parseF = parseTmArith

-- Test

s :: Syntactic '[TmBool]
s = CCons CVoid

run = runP s
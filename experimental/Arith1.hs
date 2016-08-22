{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PolyKinds             #-}

module Arith (TmBool(..), TmNat(..), TmArith(..)) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)
import           Data.Typeable
import           GHC.TypeLits

-- TmBool

data TmBool e l where
  TmTrue :: TmBool e 0
  TmFalse :: TmBool e 0
  TmIf :: e 0 -> e 0 -> e 0 -> TmBool e 0

instance HFunctor TmBool where
  hfmap _ TmTrue = TmTrue
  hfmap _ TmFalse = TmFalse
  hfmap f (TmIf e1 e2 e3) = TmIf (f e1) (f e2) (f e3)

parseTmBool :: NewParser TmBool fs 0
parseTmBool e p =
  (keyword "true" >> pure (In e TmTrue)) <|>
  (keyword "false" >> pure (In e TmFalse)) <|>
  do { keyword "if"; e1 <- p !!! (Proxy :: Proxy 0);
       keyword "then"; e2 <- p !!! (Proxy :: Proxy 0);
       keyword "else"; e3 <- p !!! (Proxy :: Proxy 0);
       return $ In e (TmIf e1 e2 e3)}

instance Syntax TmBool 0 where
  keywords _ _ = ["true", "false", "if", "then", "else"]
  parseF   _   = parseTmBool
  -- prettyF _ TmTrue          = text "true"
  -- prettyF _ TmFalse         = text "false"
  -- prettyF r (TmIf e1 e2 e3) = parens $ text "if " <> r e1 <> text " then " <> r e2 <> text " else " <> r e3

-- TmNat

data TmNat e l where
  TmZero :: TmNat e 0
  TmSucc :: e 0 -> TmNat e 0
  TmPred :: e 0 -> TmNat e 0

instance HFunctor TmNat where
  hfmap _ TmZero = TmZero
  hfmap f (TmSucc e) = TmSucc (f e)
  hfmap f (TmPred e) = TmPred (f e)
  
parseTmNat :: NewParser TmNat fs 0
parseTmNat e p =
  (keyword "0" >> pure (In e TmZero)) <|>
  (keyword "succ" >> (In e . TmSucc) <$> (p !!! (Proxy :: Proxy 0))) <|>
  (keyword "pred" >> (In e . TmPred) <$> (p !!! (Proxy :: Proxy 0)))

instance Syntax TmNat 0 where
  keywords _ _ = ["0", "succ", "pred"]
  parseF   _   = parseTmNat
  -- prettyF _ TmZero     = text "0"
  -- prettyF r (TmSucc e) = text "succ" <+> parens (r e)
  -- prettyF r (TmPred e) = text "pred" <+> parens (r e)

-- TmArith

data TmArith e l where
  TmIsZero :: e 0 -> TmArith e 0

instance HFunctor TmArith where
  hfmap f (TmIsZero e) = TmIsZero (f e)

parseTmArith :: NewParser TmArith fs 0
parseTmArith e p = keyword "iszero" >> (In e . TmIsZero <$> (p !!! (Proxy :: Proxy 0)))

instance Syntax TmArith 0 where
  keywords _ _ = ["iszero"]
  parseF   _   = parseTmArith
  -- prettyF r (TmIsZero e) = text "iszero" <+> parens (r e)

s :: Syntactic '[TmBool, TmNat, TmArith] '[0, 0, 0]
s = crep

r = runP s (Proxy :: Proxy 0)

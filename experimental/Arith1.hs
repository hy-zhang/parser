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

module Arith1 (TmBool(..), TmNat(..), TmArith(..)) where

import           Lib
import           Text.Parsec      hiding (runP)
import           Text.PrettyPrint hiding (char, space)
import           Data.Typeable
import           GHC.TypeLits
import qualified Data.Map         as M

-- TmBool

data TmBool e l where
  TmTrue :: TmBool e 0
  TmFalse :: TmBool e 0
  TmIf :: e 0 -> e 0 -> e 0 -> TmBool e 0

instance HFunctor TmBool where
  hfmap _ TmTrue = TmTrue
  hfmap _ TmFalse = TmFalse
  hfmap f (TmIf e1 e2 e3) = TmIf (f e1) (f e2) (f e3)

instance MyShow TmBool where
  showMe TmTrue = "true"
  showMe TmFalse = "false"
  showMe (TmIf (In _ e1) (In _ e2) (In _ e3)) =
    "if (" ++ showMe e1 ++ ") then (" ++ showMe e2 ++ ") else (" ++ showMe e3 ++ ")"

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

-- TmNat

data TmNat e l where
  TmZero :: TmNat e 0
  TmSucc :: e 0 -> TmNat e 0
  TmPred :: e 0 -> TmNat e 0

instance HFunctor TmNat where
  hfmap _ TmZero = TmZero
  hfmap f (TmSucc e) = TmSucc (f e)
  hfmap f (TmPred e) = TmPred (f e)

instance MyShow TmNat where
  showMe TmZero = "0"
  showMe (TmSucc (In _ e)) = "succ (" ++ showMe e ++ ")"
  showMe (TmPred (In _ e)) = "pred (" ++ showMe e ++ ")"
  
parseTmNat :: NewParser TmNat fs 0
parseTmNat e p =
  (keyword "0" >> pure (In e TmZero)) <|>
  (keyword "succ" >> (In e . TmSucc) <$> (p !!! (Proxy :: Proxy 0))) <|>
  (keyword "pred" >> (In e . TmPred) <$> (p !!! (Proxy :: Proxy 0)))

instance Syntax TmNat 0 where
  keywords _ _ = ["0", "succ", "pred"]
  parseF   _   = parseTmNat

-- TmArith

data TmArith e l where
  TmIsZero :: e 0 -> TmArith e 0

instance HFunctor TmArith where
  hfmap f (TmIsZero e) = TmIsZero (f e)

instance MyShow TmArith where
  showMe (TmIsZero (In _ e)) = "iszero (" ++ showMe e ++ ")"

parseTmArith :: NewParser TmArith fs 0
parseTmArith e p = keyword "iszero" >> (In e . TmIsZero <$> (p !!! (Proxy :: Proxy 0)))

instance Syntax TmArith 0 where
  keywords _ _ = ["iszero"]
  parseF   _   = parseTmArith

-- s :: [Int] -> Syntactic '[TmBool, TmNat, TmArith] '[0, 0, 0]
-- s (x : y : z : zs) = CCons x $ CCons y $ CCons z $ CVoid

-- r = testRun s 3 (Proxy :: Proxy 0)
-- r' = test (s [1,2,3]) (Proxy :: Proxy 0)
  -- [("0", "0"),
   -- ("succ (pred 0)", "succ (pred (0))"),
   -- ("iszero (pred (succ (succ 0)))", "iszero (pred (succ (succ (0))))")]

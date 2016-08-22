{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FullIsoRec (TmFold(..)) where

import           Lib
import           Text.Parsec
import           Text.PrettyPrint hiding (char, space)


-- TmFold

data TmFold e = TmFold e e | TmUnFold e e deriving (Functor, Show)

parseTmFold :: NewParser TmFold fs
parseTmFold e p = pTmFold <|> pTmUnFold
  where
    pTmFold = keyword "fold" >> pt TmFold
    pTmUnFold = keyword "unfold" >> pt TmUnFold
    pt constructor = do
      keyword "["
      t <- p
      keyword "]"
      expr <- p
      return $ In e (constructor t expr)


instance Syntax TmFold where
  keywords _ = ["fold", "unfold"]
  parseF = parseTmFold
  prettyF r expr =
    case expr of
      (TmFold t e) -> "fold" <+> "[" <> r t <> "]" <+> r e
      (TmUnFold t e) -> "unfold" <+> "[" <> r t <> "]" <+> r e

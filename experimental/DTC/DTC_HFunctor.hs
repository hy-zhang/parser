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
{-# LANGUAGE RankNTypes  #-}

data A
data B
data ISig e l where
  Const :: Int -> ISig e A
  Mult  :: e A -> e A -> ISig e A
  Fst, Snd :: e B -> ISig e A

data PSig e l where
  Pair :: e A -> e A -> PSig e B

data Fix f l = In (f (Fix f) l)

type f ==> g = forall a. f a -> g a

class HFunctor h where
  hfmap :: f ==> g -> h f ==> h g

instance HFunctor ISig where
  hfmap _ (Const i) = Const i
  hfmap f (Mult x y) = Mult (f x) (f y)
  hfmap f (Fst x) = Fst (f x)
  hfmap f (Snd x) = Snd (f x)

instance HFunctor PSig where
  hfmap f (Pair x y) = Pair (f x) (f y)

data (f :+: g) (a :: * -> *) l = Inl (f a l) | Inr (g a l)

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
  hfmap h (Inl x) = Inl (hfmap h x)
  hfmap h (Inr x) = Inr (hfmap h x)

type AExpr = Fix (ISig :+: PSig) A
type BExpr = Fix (ISig :+: PSig) B

e1 :: AExpr
e1 = In (Inl (Const 3))

e2 :: AExpr
e2 = In (Inl (Const 4))

e3 :: AExpr
e3 = In (Inl (Mult e1 e2))

e4 :: BExpr
e4 = In (Inr (Pair e1 e2))

e5 :: AExpr
e5 = In (Inl (Fst e4))

type Alg f a = f a ==> a

fold :: HFunctor f => Alg f a -> Fix f ==> a
fold f (In t) = f (hfmap (fold f) t)

class HFunctor f => Eval f where
  evalAlg :: f FString ==> FString

data FString e = FString String deriving Show

mult :: FString e1 -> FString e2 -> FString e3
mult (FString x) (FString y) = FString ("(" ++ x ++ " * " ++ y ++ ")")

pair :: FString e1 -> FString e2 -> FString e3
pair (FString x) (FString y) = FString ("(" ++ x ++ ", " ++ y ++ ")")

fstP :: FString e1 -> FString e2
fstP (FString x) = FString ("fst " ++ x)

sndP :: FString e1 -> FString e2
sndP (FString x) = FString ("snd " ++ x)

instance Eval ISig where
  evalAlg (Const i) = FString (show i)
  evalAlg (Mult x y) = mult x y
  evalAlg (Fst x) = fstP x
  evalAlg (Snd x) = sndP x

instance Eval PSig where
  evalAlg (Pair x y) = pair x y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg (Inl x) = evalAlg x
  evalAlg (Inr x) = evalAlg x

eval :: Eval f => Fix f ==> FString
eval = fold evalAlg

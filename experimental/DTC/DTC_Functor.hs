{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DeriveFunctor  #-}

data (f :+: g) e = Inl (f e) | Inr (g e) deriving Functor

data Fix f = In (f (Fix f))

data Val e = Val Int deriving Functor
data Add e = Add e e deriving Functor

class Functor f => Eval f where
  eval :: f Int -> Int

instance Eval Val where
  eval (Val x) = x

instance Eval Add where
  eval (Add e1 e2) = e1 + e2

instance (Eval f, Eval g) => Eval (f :+: g) where
  eval (Inl x) = eval x
  eval (Inr x) = eval x

foldE :: Functor f => (f a -> a) -> Fix f -> a
foldE f (In t) = f (fmap (foldE f) t)

r :: Eval f => Fix f -> Int
r = foldE eval

example :: Fix (Val :+: Add)
example = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))
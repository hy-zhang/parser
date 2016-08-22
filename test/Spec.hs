{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Test.Hspec
import           Test.QuickCheck

import           Arith
import           FullUntyped
import           Lib
import           SimpleBool
import           TyArith
import           Untyped
import           FullSimple
import           FullRef


test :: (fs :< fs) => Syntactic fs -> String -> [String] -> [String] -> SpecWith ()
test fs des input output =
  describe des (sequence_ ts)
  where
    ts = zipWith3 (\i r o -> it i $ r `shouldBe` o) input result output
    result = map (runP' fs) input


testArith :: SpecWith ()
testArith = test s "Arith" input output
  where
    s :: Syntactic '[TmBool, TmNat, TmArith]
    s = crep
    input =
      [ "true"
      , "if false then true else false"
      , "0"
      , "succ (pred 0)"
      , "iszero (pred (succ (succ 0)))"]
    output =
      [ "true"
      , "(if false then true else false)"
      , "0"
      , "succ (pred (0))"
      , "iszero (pred (succ (succ (0))))"]


testUntyped :: SpecWith ()
testUntyped = test s "Untyped" input output
  where
    s :: Syntactic '[TmApp, TmLam, TmVar]
    s = crep
    input = [
      "x",
      "y'",
      "\\x.x",
      "(\\x.x)   \\x.x",
      "\\x.(x)",
      "\\x.(x x)",
      "\\x.x \\x.x"]
    output = [
      "x",
      "y'",
      "\\x.x",
      "(\\x.x \\x.x)",
      "\\x.x",
      "\\x.(x x)",
      "(\\x.x \\x.x)"]


testFullUntyped :: SpecWith ()
testFullUntyped = test s "FullUntyped" input output
  where
    s :: Syntactic '[TmBool, TmNat, TmArith, TmApp, TmLam, TmLet, TmRecord, TmFloat, TmString, TmVar]
    s = crep
    input = [
      "x",
      "y'",
      "if x then false else x",
      "\\x.x",
      "(\\x.x)   \\x.x",
      "\\x.(x)",
      "\\x.(x x)",
      "\\x.x \\x.x",
      "2.0",
      "timesfloat 2.0 3.0",
      "timesfloat (timesfloat 2.0 3.0) (timesfloat 4.0 5.0)",
      "\"hello\"",
      "0",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "let x=true in x",
      "{x=\\x.x, y=(\\x.x)(\\x.x)}",
      "{x=\\x.x, y=(\\x.x)(\\x.x)}.x",
      "{x=1.0}.t"]
    output = [
      "x",
      "y'",
      "(if x then false else x)",
      "\\x.x",
      "(\\x.x \\x.x)",
      "\\x.x",
      "\\x.(x x)",
      "(\\x.x \\x.x)",
      "2.0",
      "timesfloat (2.0) (3.0)",
      "timesfloat (timesfloat (2.0) (3.0)) (timesfloat (4.0) (5.0))",
      "\"hello\"",
      "0",
      "succ (pred (0))",
      "iszero (pred (succ (succ (0))))",
      "let x = true in x",
      "{x=\\x.x, y=(\\x.x \\x.x)}",
      "{x=\\x.x, y=(\\x.x \\x.x)}.x",
      "{x=1.0}.t"]

testTyArith :: SpecWith ()
testTyArith = test s "TyArith" input output
  where
    s :: Syntactic '[TmBool, TmNat, TmArith, TyNat, TyBool]
    s = crep
    input = [
      "true",
      "if false then true else false",
      "0",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "Nat",
      "Bool"]
    output = [
      "true",
      "(if false then true else false)",
      "0",
      "succ (pred (0))",
      "iszero (pred (succ (succ (0))))",
      "Nat",
      "Bool"]

testSimpleBool :: SpecWith ()
testSimpleBool = test s "SimpleBool" input output
  where
    s :: Syntactic '[TyArr, TmBool, TyBool, TmApp, TmLam2, TmVar]
    s = crep
    input = [
      "true",
      "if false then true else false",
      "if (x) then true else false",
      "Bool",
      "\\x:Bool.x",
      "Bool->Bool",
      "(\\x:Bool->Bool.x)",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)"]
    output = [
      "true",
      "(if false then true else false)",
      "(if x then true else false)",
      "Bool",
      "\\x:Bool.x",
      "Bool->Bool",
      "\\x:Bool->Bool.x",
      "(\\x:Bool->Bool.(if (x false) then true else false) \\x:Bool.(if x then false else true))"]

testFullSimple :: SpecWith ()
testFullSimple = test s "FullSimple" input output
  where
    s :: Syntactic '[TmApp, TyArr, TmCase, TmRecord, TmFloat, TmLet, TmString, TmLam2, TmBool, TmNat, TmArith, TmAscribe, TyVariant, TmTag, TmUnit, TyUnit, TyBool, TyNat, TmVar]
    s = crep
    input = [
      "x",
      "if x then false else x",
      "\\x:A.x",
      "timesfloat 2.0 3.0",
      "\"hello\"",
      "0",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "true",
      "if false then true else false",
      "if (x) then true else false",
      "Bool",
      "\\x:Bool.x",
      "Bool->Bool",
      "(\\x:Bool->Bool.x)",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)",
      "\\x:Nat.succ x",
      "(\\x:Nat. succ (succ x)) (succ 0)",
      "\\f:T. \\x:Nat. f (f x)",
      "let x=true in x",
      "{x=1.0}.t",
      "Unit",
      "unit",
      "unit as Unit",
      "<l=unit> as <l:Unit, r:Unit>",
      "case a of <phy=x> => x.first | <vir=y> => y.name"]
    output = [
      "x",
      "(if x then false else x)",
      "\\x:A.x",
      "timesfloat (2.0) (3.0)",
      "\"hello\"",
      "0",
      "succ (pred (0))",
      "iszero (pred (succ (succ (0))))",
      "true",
      "(if false then true else false)",
      "(if x then true else false)",
      "Bool",
      "\\x:Bool.x",
      "Bool->Bool",
      "\\x:Bool->Bool.x",
      "(\\x:Bool->Bool.(if (x false) then true else false) \\x:Bool.(if x then false else true))",
      "\\x:Nat.succ (x)",
      "(\\x:Nat.succ (succ (x)) succ (0))",
      "(\\f:T.\\x:Nat.f (f x))",
      "let x = true in x",
      "{x=1.0}.t",
      "Unit",
      "unit",
      "(unit as Unit)",
      "(<l=unit> as <l:Unit, r:Unit>)",
      "case a of <phy=x> => x.first | <vir=y> => y.name"]

testFullRef :: SpecWith ()
testFullRef = test s "FullRef" input output
  where
    s :: Syntactic '[TmApp, TmAssign, TyArr, TmLet, TmLam2, TmRef, TmBool, TmDeref, TyRef, TySink, TySource, TmNat, TyNat, TyUnit, TmVar]
    s = crep
    input = [
      "\\a:Ref (Nat->Nat).\\n:Nat.(!a n)",
      "\\a:Unit.ref (\\n:Nat.0)",
      "\\a:Ref (Nat->Nat).\\m:Nat.\\n:Nat.let oldf = !a in a := (\\n:Nat.if true then v else (oldf n))",
      "\\x:Top.x",
      "let t = Source Nat in \\x:t.unit",
      "\\x:Sink t.unit"]
    output = [
      "\\a:Ref Nat->Nat.\\n:Nat.((!a) n)",
      "\\a:Unit.ref \\n:Nat.0",
      "\\a:Ref Nat->Nat.\\m:Nat.\\n:Nat.let oldf = (!a) in a := \\n:Nat.(if true then v else (oldf n))",
      "\\x:Top.x",
      "let t = Source Nat in \\x:t.unit",
      "\\x:Sink t.unit"]


main :: IO ()
main = hspec $ do
  testArith
  testUntyped
  testFullUntyped
  testTyArith
  testSimpleBool
  testFullSimple
  testFullRef

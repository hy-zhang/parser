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
import           FullError
import           RcdSubBot
import           FullEquiRec
import           FullIsoRec
import           FullPoly


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
    s :: Syntactic '[TmApp, TyArr, TmCase, TmRecord, TmFloat, TmLet, TmFix, TmString, TmLam2, TmBool, TmNat, TmArith, TmAscribe, TyVariant, TmTag, TmUnit, TyUnit, TyBool, TyNat, TyString, TyVar, TmVar]
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
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "\\a:Unit.fix (\\x:T.x)"]
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
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "\\a:Unit.fix (\\x:T.x)"]

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

testFullError :: SpecWith ()
testFullError = test s "FullError" input output
  where
    s :: Syntactic '[TmApp, TmBool, TmLam2, TyTop, TyBot, TmError, TmTry, TyBool, TmVar]
    s = crep
    input = [
      "\\x:Top.if error then (try x with true) else false",
      "error true",
      "(\\x:Bool.x) error"]
    output = [
      "\\x:Top.(if error then try x with true else false)",
      "(error true)",
      "(\\x:Bool.x error)"]

testRcdSubBot :: SpecWith ()
testRcdSubBot = test s "RcdSubBot" input output
  where
    s :: Syntactic '[TmApp, TyArr, TmBool, TmLam2, TyRecord, TmRecord, TyTop, TyBot, TmVar]
    s = crep
    input = [
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x"]
    output = [
      "\\x:Top.x",
      "((\\r:{x:Top->Top}.r.x r.x) {x=\\z:Top.z, y=\\z:Top.z})",
      "(\\x:Bot.x x)"]

testFullSub :: SpecWith ()
testFullSub = test s "FullSub" input output
  where
    s :: Syntactic '[TmApp, TyArr, TmCase, TmRecord, TyRecord, TmFloat, TmLet, TmFix, TmString, TmLam2, TmBool, TmNat, TmArith, TmAscribe, TmAssign, TyVariant, TmTag, TmTry, TmError, TyRef, TmRef, TmDeref, TySource ,TySink, TmUnit, TyUnit, TyBool, TyNat, TyTop, TyBot, TyString, TyVar, TmVar]
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
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x",
      "\\a:Ref (Nat->Nat).\\n:Nat.(!a n)",
      "\\a:Unit.ref (\\n:Nat.0)",
      "\\a:Ref (Nat->Nat).\\m:Nat.\\n:Nat.let oldf = !a in a := (\\n:Nat.if true then v else (oldf n))",
      "let t = Source Nat in \\x:t.unit",
      "\\x:Sink t.unit",
      "\\x:Top.if error then (try x with true) else false",
      "error true",
      "(\\x:Bool.x) error"]
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
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "\\x:Top.x",
      "((\\r:{x:Top->Top}.r.x r.x) {x=\\z:Top.z, y=\\z:Top.z})",
      "(\\x:Bot.x x)",
      "\\a:Ref Nat->Nat.\\n:Nat.((!a) n)",
      "\\a:Unit.ref \\n:Nat.0",
      "\\a:Ref Nat->Nat.\\m:Nat.\\n:Nat.let oldf = (!a) in a := \\n:Nat.(if true then v else (oldf n))",
      "let t = Source Nat in \\x:t.unit",
      "\\x:Sink t.unit",
      "\\x:Top.(if error then try x with true else false)",
      "(error true)",
      "(\\x:Bool.x error)"]

testFullEquiRec :: SpecWith ()
testFullEquiRec = test s "FullEquiRec" input output
  where
    s :: Syntactic '[TmApp, TyArr, TmCase, TmRecord, TyRecord, TmFloat, TmLet, TmFix, TmString, TmLam2, TmBool, TmNat, TmArith, TmAscribe, TmAssign, TyVariant, TmTag, TmTry, TmError, TyRec, TyRef, TmRef, TmDeref, TySource ,TySink, TmUnit, TyUnit, TyBool, TyNat, TyTop, TyBot, TyString, TyVar, TmVar]
    s = crep
    input = [
      "\\f:(Rec X.A->A).\\x:A.f x",
      "\\x:<a:Bool, b:Bool>.x",
      "Rec P.{get:Nat, inc:Unit->P}",
      "Rec A.Nat->A",
      "let g = fix (\\f:Nat->(Rec A.Nat->A).\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.if iszero m then n else succ (p (pred m) n))"]
    output = [
      "(\\f:Rec X.A->A.\\x:A.f x)",
      "\\x:<a:Bool, b:Bool>.x",
      "Rec P.{get:Nat, inc:Unit->P}",
      "Rec A.Nat->A",
      "let g = fix (\\f:Nat->Rec A.Nat->A.\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.(if iszero (m) then n else succ (((p pred (m)) n))))"]

testFullIsoRec :: SpecWith ()
testFullIsoRec = test s "FullIsoRec" input output
  where
    s :: Syntactic '[TmApp, TyArr, TmCase, TmRecord, TyRecord, TmFloat, TmLet, TmFix, TmString, TmLam2, TmBool, TmFold, TmNat, TmArith, TmAscribe, TmAssign, TyVariant, TmTag, TmTry, TmError, TyRec, TyRef, TmRef, TmDeref, TySource ,TySink, TmUnit, TyUnit, TyBool, TyNat, TyTop, TyBot, TyString, TyVar, TmVar]
    s = crep
    input = [
      "let Counter = Rec P.{get:Nat, inc:Unit->P} in fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get"]
    output = [
      "let Counter = Rec P.{get:Nat, inc:Unit->P} in fold [Counter] {get=unit, inc=unit}",
      "unfold [Counter] p.get"]

testFullPoly :: SpecWith ()
testFullPoly = test s "FullPoly" input output
  where
    s :: Syntactic '[TmTApp, TmApp, TyArr, TmCase, TmRecord, TyRecord, TmPack, TmUnpack, TyAll, TySome, TmFloat, TmLet, TmFix, TmString, TmTAbs, TmLam2, TmBool, TmFold, TmNat, TmArith, TmAscribe, TmAssign, TyVariant, TmTag, TmTry, TmError, TyRec, TyRef, TmRef, TmDeref, TySource ,TySink, TmUnit, TyUnit, TyBool, TyNat, TyTop, TyBot, TyString, TyVar, TmVar]
    s = crep
    input = [
      "let Counter = Rec P.{get:Nat, inc:Unit->P} in fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get",
      "x",
      "if x then false else x",
      "\\x:A.x",
      "timesfloat 2.0 3.0",
      "\"hello\"",
      "(\\X.\\x:X.x) [All X.X->X]",
      "{Some X, {c:X, f:X->Nat}}",
      "{*All Y.Y, \\x:(All Y.Y).x} as {Some X, X->X}",
      "{*Nat, {c=0, f=\\x:Nat. succ x}} as {Some X, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ x}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)"]
    output = [
      "let Counter = Rec P.{get:Nat, inc:Unit->P} in fold [Counter] {get=unit, inc=unit}",
      "unfold [Counter] p.get",
      "x",
      "(if x then false else x)",
      "\\x:A.x",
      "timesfloat (2.0) (3.0)",
      "\"hello\"",
      "(\\X.\\x:X.x [All X.X->X])",
      "{Some X, {c:X, f:X->Nat}}",
      "{*All Y.Y, \\x:All Y.Y.x} as {Some X, X->X}",
      "{*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)"]


main :: IO ()
main = hspec $ do
  testArith
  testUntyped
  testFullUntyped
  testTyArith
  testSimpleBool
  testFullSimple
  testFullRef
  testFullError
  testRcdSubBot
  testFullSub
  testFullEquiRec
  testFullIsoRec
  testFullPoly

# Extensible parser

## Case study

### Features

Name              | # of structs | Based on
----------------- | ------------ | -----------
Bool              | 3            |
Nat               | 4            |
VarApp            | 2            |
UntypedAbs        | 1            |
Record            | 2            |
FloatString       | 3            |
Let               | 1            |
Typed             | 4 = 2 + 2    | VarApp
TypedBool         | 4 = 3 + 1    | Bool
TypedNat          | 5 = 4 + 1    | Nat
TypeVar           | 1            |
TypedRecord       | 3 = 2 + 1    | Record
Variant           | 3            |
Extension         | 7            |
Simple            | 28           | TyArith, Typed, FloatString, Let, TypedRecord, Extension, TypeVar
Top               | 1            |
TopBot            | 2 = 1 + 1    | Top
Ref               | 6            |
Error             | 2            |
RecType           | 1            |
Fold              | 2            |
Pack              | 2            |
Poly              | 4            |
Omega             | 8            |

### Languages

Name              | Component
----------------- | -------------------------------
Arith             | Bool, Nat
Untyped           | VarApp, UntypedAbs
FullUntyped       | Arith, Untyped, Record, FloatString, Let
TyArith           | TypedBool, TypedNat
SimpleBool        | Typed, TypedBool
FullSimple        | Simple, Variant
Bot               | Typed, TopBot
FullRef           | FullSimple, TopBot, Ref, SourceSink
FullError         | Bot, TypedBool, Error, TypeVar
RcdSubBot         | Bot, TypedRecord
FullSub           | Simple, Top
FullEquiRec       | FullSimple, RecType
FullIsoRec        | FullEquiRec, Fold
EquiRec           | Typed, RecType, TypeVar
Recon             | Typed, TyArith, TypeVar
FullRecon         | Recon, Let
FullPoly          | Simple, Pack, Poly
FullOmega         | Simple, Ref, Pack, Omega

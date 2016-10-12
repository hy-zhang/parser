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
FullUntyped Ext   | 4            |
Typed             | 4 = 2 + 2    | VarApp
TypedBool         | 4 = 3 + 1    | Bool
TypedNat          | 5 = 4 + 1    | Nat
TypeVar           | 1            |
TypedRecord       | 3 = 2 + 1    | Record
FullSimple Ext    |              |
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
FullUntyped       | Arith, Untyped, Record, FullUntyped Ext
TyArith           | TypedBool, TypedNat
SimpleBool        | Typed, TypedBool
FullSimple        | Typed, TyArith, FullUntyped Ext, FullSimple Ext
Bot               | Typed, TopBot
FullRef           | FullSimple, TopBot, Ref
FullError         | Bot, TypeBool, Error, TypeVar
RcdSubBot         | Bot, TypedRecord
FullSub           | FullSimple, Top
FullEquiRec       | FullSimple, RecType
FullIsoRec        | FullEquiRec, Fold
EquiRec           | Typed, RecType, TypeVar
Recon             | Typed, TyArith, TyVar
FullRecon         | 
FullPoly          | FullSimple, Pack, Poly
FullOmega         | FullSimple, Ref, Pack, Omega

-- |

module Language.SimpleGo.Balsa.Builtins (
  types, functions,
  bool,
  byte, rune, int, uint,
  uint8, uint16, uint32, uint64,
  int8, int16, int32, int64,
  string
  ) where

import qualified ParseTree as PT
import qualified Report    as R

data Signed = Signed | Unsigned

bool :: PT.Type
bool = PT.Bits 1

byte = uint8
rune = int32
int = int32
uint = uint32

uint8 :: PT.Type
uint8 = convert Unsigned 8

uint16 :: PT.Type
uint16 = convert Unsigned 16

uint32 :: PT.Type
uint32 = convert Unsigned 32

uint64 :: PT.Type
uint64 = convert Unsigned 64

int8 :: PT.Type
int8 = convert Signed 8

int16 :: PT.Type
int16 = convert Signed 16

int32 :: PT.Type
int32 = convert Signed 32

int64 :: PT.Type
int64 = convert Signed 64

string = builtin "String"

--float32
--float64     the set of all IEEE-754 64-bit floating-point numbers

--complex64   the set of all complex numbers with float32 real and imaginary parts
--complex128  the set of all complex numbers with float64 real and imaginary parts

pos :: R.Pos
pos = R.NoPos

convert :: Signed -> Integer -> PT.Type
convert signed bits = PT.NumType pos (PT.ValueExpr pos (PT.Bits 8) (PT.IntValue bits)) (isSigned signed)
  where
    isSigned Signed = True
    isSigned _ = False


alias :: String -> PT.Type
alias = PT.NameType pos

builtin :: String -> PT.Type
builtin = PT.BuiltinType

types :: [(String, PT.Type)]
types = [
  ("bool", bool),
  ("uint8", uint8),
  ("uint16", uint16),
  ("uint32", uint32),
  ("uint64", uint64),
  ("int8", int8),
  ("int16", int16),
  ("int32", int32),
  ("int64", int64),
  ("uint", uint32),
  ("int", int32),
  ("byte", alias "uint8"),
  ("rune", alias "int32"),
  ("string", string)
  ]

smash :: PT.Type -> PT.Expr -> PT.Expr
smash = PT.CastExpr pos

unsmash :: PT.Expr -> PT.Expr
unsmash = PT.UnExpr pos PT.NoType PT.UnSmash

functions :: [(String, PT.Expr -> PT.Expr)]
functions = [
  ("smashU32", smash uint32),
  ("smashU8", smash uint8),
  ("unsmashU8", unsmash),
  ("unsmashU32", unsmash)
  ]

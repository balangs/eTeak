-- |

module Language.SimpleGo.AST (
  Program(..),
  Id(..),
  Declaration(..),
  Rec(..),
  Signature(..),
  Param(..),
  Type(..),
  ChanKind(..),
  Expr(..),
  Prim(..),
  Value(..),
  Block(..),
  Statement(..),
  Simp(..),
  Chan(..),
  Cond(..),
  Case(..),
  -- rexports
  Operators.Unary(..),
  Operators.Binary(..)
  ) where

import qualified Data.Text                       as T
import qualified Data.Vector                     as U
import qualified Language.SimpleGo.AST.Operators as Operators

newtype PackageName = PackageName { unPackage :: T.Text } deriving (Eq, Show)

data Program = Program {
      declarations :: U.Vector Declaration
      } deriving (Eq, Show, Read)

newtype Id = Id { idText :: T.Text } deriving (Eq, Show, Read)


data Declaration = Const Id Type Expr
                 | Var Id Type Expr
                 | Type Id Type
                 | Func Id Signature Block
                 deriving (Eq, Read, Show)

data Rec = Rec Bool (Maybe Id) Type
         deriving (Eq, Read, Show)

data Signature = Signature {
  input  :: U.Vector Param,
  output :: U.Vector Param
  } deriving (Eq, Read, Show)

data Param = Param Id Type
           deriving (Eq, Read, Show)

data Type = TypeName Id
          | ArrayType Expr Type
          | Channel ChanKind Type
          | FunctionType Signature
          | MapType Type Type
          | PointerType Type
          | SliceType Type
          | EllipsisType Type -- only in Literals
          | VariadicType Type -- only in Funcs
          deriving (Eq, Read, Show)


data ChanKind = Input         -- <-chan
              | Output        -- chan<-
              | Bidirectional -- chan
              deriving (Eq, Read, Show)

data Expr = Zero
          | Prim Prim
          | UnOp Operators.Unary Expr
          | BinOp Operators.Binary Expr Expr
          deriving (Eq, Read, Show)

data Prim = LitInt  Integer
          | LitReal Float
          | LitImag Float
          | LitChar Char
          | LitStr  String
          | LitFunc Signature Block
          | Qual Id                              -- 'PrimaryExpr/Operand/QualifiedIdent'
          | Method Rec Id                        -- 'PrimaryExpr/Operand/MethodExpr'
          | Paren Expr                           -- 'PrimaryExpr/Operand/MethodExpr'
          | Cast Type Expr                       -- 'PrimaryExpr/Conversion'
          | New  Type                            -- 'PrimaryExpr/BuiltinCall/new'
          | Make Type [Expr]                     -- 'PrimaryExpr/BuiltinCall/make'
                                                 --          | BI Id Type [Expr]  -- 'PrimaryExpr/BuiltinCall'
          | Select Prim Id                       -- 'PrimaryExpr/Selector'
          | Index Prim Expr                      -- 'PrimaryExpr/Index'
          | Slice Prim (Maybe Expr) (Maybe Expr) -- 'PrimaryExpr/Slice'
          | TA    Prim Type                      -- 'PrimaryExpr/TypeAssertion'
          | Call  Prim [Expr]
          deriving (Eq, Read, Show)

data Comp = Comp [Element]
          deriving (Eq, Read, Show)

data Element = Element Key Value
             deriving (Eq, Read, Show)

data Key = KeyNone
         | KeyField Id
         | KeyIndex Expr
         deriving (Eq, Read, Show)

data Value = ValueExpr Expr -- 'Value/Expression'
           | ValueComp Comp -- 'Value/LiteralValue'
           deriving (Eq, Read, Show)

newtype Block = Block (U.Vector Statement)
              deriving (Eq, Read, Show)

data Statement = StmtDecl Declaration -- 'Statement/Declaration'
               | Simple Simp
               | Go Expr
               | Return [Expr]
               | Break    (Maybe Id)
               | Continue (Maybe Id)
               | Fallthrough
               | StmtBlock Block
               | If Cond Block (Maybe Statement)
                 -- TODO rewrite this to a more static case structure, with default
               | StmtSelect  [Case Chan]
               | Switch Cond [Case Expr]
               | ForWhile (Maybe Expr) Block
               | ForThree Simp (Maybe Expr) Simp Block
                 -- True if AssignDecl
               | ForRange [Expr] Expr Bool Block
               deriving (Eq, Read, Show)

data Simp = Empty
          | Send Expr Expr
          | SimpleExpr Expr
          | Inc  Expr
          | Dec  Expr
          | SimpVar Id Expr
          deriving (Eq, Read, Show)

data Chan = ChanRecv (Maybe (Expr, Maybe Expr, Operators.Unary)) Expr
          | ChanSend Expr Expr
          deriving (Eq, Read, Show)

data Cond = Cond (Maybe Simp) (Maybe Expr)
          deriving (Eq, Read, Show)


data Case a = Case [a] [Statement]
            | Default  [Statement]
            deriving (Eq, Read, Show)

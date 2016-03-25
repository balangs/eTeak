-- |

module Language.SimpleGo.AST where

import qualified Data.Text   as T
import qualified Data.Vector as U


newtype PackageName = PackageName { unPackage :: T.Text } deriving (Eq, Show)

-- | Go Language source start
data Program = Program {
      declarations :: U.Vector Declaration
      } deriving (Eq, Show, Read)

newtype Id = Id T.Text deriving (Eq, Show, Read)

data Declaration = Const Id Type Expr
                 | Var Id Type Expr
                 | Type Id Type
                 | Func Id Signature Block
                deriving (Eq, Read, Show)

newtype LocalId = LocalId T.Text deriving (Eq, Show, Read)
newtype ImportPath = ImportPath T.Text deriving (Eq, Show, Read)

data Import = Import LocalId ImportPath
            deriving (Eq, Read, Show)


data CVSpec = CVSpec [Id] (Maybe Type) [Expr]
                deriving (Eq, Read, Show)

data FuncDecl  = FuncDecl       Id Signature Block
                deriving (Eq, Read, Show)

data MethDecl  = MethDecl Rec Id Signature Block
                deriving (Eq, Read, Show)

data MethSpec  = MethSpec  Id Signature
                 | IfaceName (Maybe Id) Id
                deriving (Eq, Read, Show)



data UnOp = Plus      -- '+' (0 + x)
          | Minus     -- '-' (0 - x)
          | Complement -- '^' (is m ^ x  with m = "all bits set to 1" for unsigned x and  m = -1 for signed x)
          | Not   -- '!'
          | Address  -- '*'
          | Reference       -- '&'
          | Receive     -- '<-'
                deriving (Eq, Read, Show)

data BinOp = LogicalOr -- '||'
             | LogicalAnd -- '&&'
             | Equal -- '=='
             | NotEqual -- '!='
             | LessThan -- '<'
             | LessThanEqual  -- '<='
             | GreaterThan        -- '>'
             | GreaterThanEqual        -- '>='
             | BitwiseXor       -- '^'
             | BitwiseOr       -- '|'
             | Add      -- '+'
             | Subtract     -- '-'
             | Multiply  -- '*'
             | Quotient   -- '/'
             | Remainder   -- '%'
             | LeftShift       -- '<<'
             | RightShift       -- '>>'
             | BitwiseAnd       -- '&'
             | AndNot       -- '&^'
             | Dec       -- '--'
             | Inc       -- '++'
                deriving (Eq, Read, Show)

-- Rec (= 'Receiver' = 'ReceiverType')
data Rec = Rec Bool (Maybe Id) Type
                deriving (Eq, Read, Show)

data Signature = Signature {
  input  :: U.Vector Param,
  output :: U.Vector Param
  } deriving (Eq, Read, Show)

data Param = Param Id Type
           deriving (Eq, Read, Show)

-- Type (= 'Type' = 'TypeLit' = 'LiteralType')
data Type = TypeName Id
          | ArrayType Expr Type
          | ChannelType ChanKind Type
          | FunctionType Signature
          | InterfaceType [MethSpec]
          | MapType Type Type
          | PointerType Type
          | SliceType Type
          | StructType [FieldType]
          | EllipsisType Type  -- only in Literals
          | VariadicType Type  -- only in Funcs
          deriving (Eq, Read, Show)



data ChanKind = Input  -- <-chan
              | Output  -- chan<-
              | Bidirectional -- chan
              deriving (Eq, Read, Show)

-- FieldType
data FieldType = FieldType {
      getFieldTag  :: Maybe Lit,
      getFieldId   :: [Id],
      getFieldType :: Type }
                 | FieldAnon {
      getFieldTag  :: Maybe Lit,
      getFieldPtr  :: Bool,
      getFieldType :: Type } -- MUST be typename
                deriving (Eq, Read, Show)

{-  In the phrases below the symbol / means "is the only production which uses"
In the phrases below the symbol - means "is NOT the only production which uses"
InterfaceType

PrimaryExpr/Operand
PrimaryExpr/Conversion
PrimaryExpr/BuiltinCall
PrimaryExpr/Selector
PrimaryExpr/Index
PrimaryExpr/Slice
PrimaryExpr/TypeAssertion
PrimaryExpr/Call/ArgumentList

LiteralType - ArrayType
FunctionType - Signature
FunctionDecl - Signature
MethodSpec - Signature
MethodDecl - Signature
-}

-- Expr (= 'Expression')
data Expr = Zero
          | Prim Prim           -- 'PrimaryExpr'
          | UnOp UnOp Expr       -- 'Expression/UnaryExpr[2]'
          | BinOp BinOp Expr Expr -- 'Expression[2]'
          deriving (Eq, Read, Show)

-- PrimExpr (= 'PrimaryExpr')
data Prim = Literal Lit         -- 'PrimaryExpr/Operand/Literal'
            | Qual (Maybe Id) Id -- 'PrimaryExpr/Operand/QualifiedIdent'
            | Method Rec Id     -- 'PrimaryExpr/Operand/MethodExpr'
            | Paren Expr          -- 'PrimaryExpr/Operand/MethodExpr'
            | Cast Type Expr    -- 'PrimaryExpr/Conversion'
            | New  Type           -- 'PrimaryExpr/BuiltinCall/new'
            | Make Type [Expr]  -- 'PrimaryExpr/BuiltinCall/make'
--            | BI Id Type [Expr]  -- 'PrimaryExpr/BuiltinCall'
            | Select Prim Id    -- 'PrimaryExpr/Selector'
            | Index Prim Expr   -- 'PrimaryExpr/Index'
            | Slice Prim (Maybe Expr) (Maybe Expr) -- 'PrimaryExpr/Slice'
            | TA    Prim Type   -- 'PrimaryExpr/TypeAssertion'
            | Call  Prim [Expr] Bool -- 'PrimaryExpr/Call'
              deriving (Eq, Read, Show)

-- TODO merge Lit with Prim
-- Lit (= 'Literal') is only used in one place, operands
data Lit = LitInt  Integer
         | LitReal Float
         | LitImag Float
         | LitChar Char
         | LitStr  String
         deriving (Eq, Read, Show)

-- Comp (= 'CompositeLit/LiteralValue') is used in 2 places
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

data Block = Block { getStmt::[Stmt] }
             | NoBlock
                deriving (Eq, Read, Show)

data ForClause = ForWhile (Maybe Expr)
                 | ForThree Simp (Maybe Expr) Simp
                 | ForRange [Expr] Expr Bool -- True if AssignDecl
                deriving (Eq, Read, Show)

data Stmt = StmtDecl Declaration -- 'Statement/Declaration'
            | StmtLabeled Id Stmt
            | StmtSimple Simp
            | Stmt Expr
            | StmtReturn [Expr]
            | StmtBreak    (Maybe Id)
            | StmtContinue (Maybe Id)
            | Stmtto Id
            | StmtFallthrough
            | StmtBlock Block
            | StmtIf Cond Block (Maybe Stmt)
            | StmtSelect            [Case Chan]
            | StmtSwitch     Cond [Case Expr]
            | StmtTypeSwitch Cond [Case Type] (Maybe Id)
            | StmtFor ForClause Block
            | StmtDefer Expr
              deriving (Eq, Read, Show)

data Simp = SimpEmpty
            | SimpSend Expr Expr -- SimpleStmt/SendStmt
            | SimpExpr Expr        -- SimpleStmt/ExpressionStmt
            | SimpInc  Expr        -- SimpleStmt/IncDecStmt[1]
            | SimpDec  Expr        -- SimpleStmt/IncDecStmt[2]
--            | SimpAsn [Expr] Op [Expr] -- Assignment
            | SimpVar [Id] [Expr]      -- ShortVarDecl
              deriving (Eq, Read, Show)

data Chan = ChanRecv (Maybe (Expr, Maybe Expr, UnOp)) Expr
            | ChanSend Expr Expr
              deriving (Eq, Read, Show)

data Cond = Cond (Maybe Simp) (Maybe Expr)
              deriving (Eq, Read, Show)

data Case a = Case [a] [Stmt]
              | Default  [Stmt]
                deriving (Eq, Read, Show)

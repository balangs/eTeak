-- |

module Language.SimpleGo.AST.Operators (
  Unary(..),
  Binary(..)
  ) where

data Unary = Plus       -- '+' (0 + x)
           | Minus      -- '-' (0 - x)
           | Complement -- '^' (is m ^ x  with m = "all bits set to 1" for unsigned x and  m = -1 for signed x)
           | Not        -- '!'
           | Address    -- '*'
           | Reference  -- '&'
           | Receive    -- '<-'
           deriving (Eq, Read, Show)

data Binary = LogicalOr        -- '||'
            | LogicalAnd       -- '&&'
            | Equal            -- '=='
            | NotEqual         -- '!='
            | LessThan         -- '<'
            | LessThanEqual    -- '<='
            | GreaterThan      -- '>'
            | GreaterThanEqual -- '>='
            | BitwiseXor       -- '^'
            | BitwiseOr        -- '|'
            | Add              -- '+'
            | Subtract         -- '-'
            | Multiply         -- '*'
            | Quotient         -- '/'
            | Remainder        -- '%'
            | LeftShift        -- '<<'
            | RightShift       -- '>>'
            | BitwiseAnd       -- '&'
            | AndNot           -- '&^'
            deriving (Eq, Read, Show)

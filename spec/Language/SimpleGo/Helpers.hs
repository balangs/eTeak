-- |

module Language.SimpleGo.Helpers (
  parseDecl
  ) where

import qualified Language.Go.Parser.Parser as P
import qualified Language.Go.Parser.Tokens as T
import qualified Language.Go.Syntax.AST    as AST

parse :: T.GoParser a -> String -> Either String a
parse p t = case T.runGoParser p "<memory>" $ P.goTokenize t of
  Left s -> Left $ show s
  Right a -> Right a

parseDecl :: String -> Either String AST.GoDecl
parseDecl = parse P.goTopLevelDecl

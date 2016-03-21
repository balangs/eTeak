module Test (
    A (..)
    ) where

    data A = A [Int]
        | B { zz :: String }
        | C Int Int
        deriving MyTraverse

    data B = C { a :: Int, b :: Bool }
        deriving MyTraverse

    instance MyTraverse Z

    instance MyTraverse E F

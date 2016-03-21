-- Teak synthesiser for the Balsa language
-- Graph.hs : graph handling functions, modified and pared down from:

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file ../LICENCE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A version of the graph algorithms described in:
--
--   /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
--   by David King and John Launchbury.
--
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module Graph(
    Vertex,
    Edge,
    Link,
    loopLinks
    ) where
    
    import Control.Monad.ST
    import Data.Array.ST (STArray, newArray, readArray, writeArray)
    import Data.Array
    import Data.List
    import Control.Applicative

    import NetParts (NetworkLinkRef (..))

-----------------------------------
-- Data.Graph original data types
-----------------------------------

    -- SAME
    type Vertex = Int
    -- NEW
    type Link   = NetworkLinkRef

    -- SAME
    type Table a = Array Vertex a
    -- SAME
    type Graph   = Table [(Vertex, Link)]

    -- SAME
    vertices :: Graph -> [Vertex]
    vertices = indices

    -- SAME
    type Edge = (Vertex, (Vertex, Link))
    
    -- SAME
    edges :: Graph -> [Edge]
    edges g = [(v,w) | v <- vertices g, w <- g!v]
    
    -- SAME
    -- Table mapping function
    mapT :: (Vertex -> a -> b) -> Table a -> Table b
    mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]
    
    -- SAME
    type Bounds = (Vertex, Vertex)
    -- trees and forest
    --data Tree a  = Node a (Forest a)
    --type Forest a = [Tree a]    
    -- NEW, has two labels, use pair
    data Tree a b c = Node
        a    --  ^ label value
        b    --  ^ label value
        (Forest a b c) -- ^ zero or more child trees
        deriving (Eq, Read, Show)
    -- NEW, has two labels, use pair
    type Forest a b c = [Tree a b c]
    -- type Root a = Vertex

    -- SAME
    -- builds a graph form a list of links
    buildG :: Bounds -> [Edge] -> Graph
    buildG bnds es = accumArray (flip(:)) [] bnds es

    -- back edges graph
    back :: Graph -> Table Int -> Graph
    back g post = mapT select g
        where select v ws = [ (w,l) | (w,l) <- ws, post!v <= post!w ]

    -- find a list of backedges in graph-like fashion
    backEdgesF :: Graph -> [Vertex] -> [Edge]
    backEdgesF g rootNodes = edges be
        where
            be =  back g (postArr (bounds g) (dfs g (zip rootNodes (repeat (Link 0)))))

    -- Given a list of edges and a list of root nodes, find a list of back links
    backLinks :: [Edge] -> [Vertex] -> [Link]
    backLinks [] _ = []
    backLinks nodeList rootNodes =
          nub $ map (\(_,(_,l)) -> l) (backEdgesF g rootNodes)
        where
            g = buildG (elBounds nodeList) nodeList

    -- places  descendants before ancestors in trees and forests
    -- and lef subtrees before right subtreees
    postorder :: Tree Vertex b c -> [Vertex]
    postorder (Node a _ ts) = postorderF ts ++ [a]

    --- postorder flatenned
    -- SAME
    postorderF :: Forest Vertex b c -> [Vertex]
    postorderF ts = concat (map postorder ts)

    -- SAME
    tabulate :: Bounds -> [Vertex] -> Table Int
    tabulate bnds vs = array bnds (zip vs [1..])

    -- tabulates postorders
    -- NEW
    postArr :: Bounds -> Forest Vertex Link (Vertex, Link) -> Table Int
    postArr bnds ts = tabulate bnds (postorderF ts)

    -- given a graph g and a vertex v, generates a v-rooted tree
    -- containing all vertices in g reachable from v
    -- if there is any cycle the tree will be infinite :)
    -- SAME
    generate :: Graph -> (Vertex, Link) -> Tree Vertex Link (Vertex, Link)
    generate g (v,l) = Node v l (map (generate g) (g!v))

    -- SAME
    -- pruning functions
    prune    :: Bounds -> Forest Vertex Link (Vertex, Link) -> Forest Vertex Link (Vertex, Link)
    prune bnds ts = run bnds (chop ts)

    -- SAME
    chop     :: Forest Vertex Link (Vertex, Link) -> SetM s (Forest Vertex Link (Vertex, Link))
    chop []       = return []
    chop (Node v l ts : us)
          = do
        visited <- contains v
        if visited then
          chop us
         else do
          include v
          as <- chop ts
          bs <- chop us
          return (Node v l as : bs)

    -- to hold the set of vertex that we need to discard
    -- SAME
    newtype SetM s a = SetM { runSetM :: STArray s Vertex Bool -> ST s a }

    -- SAME
    instance Functor (SetM s) where
        f `fmap` SetM v = SetM $ \s -> f `fmap` v s
        {-# INLINE fmap #-}

    instance Monad (SetM s) where
        return x     = SetM $ const (return x)
        {-# INLINE return #-}
        SetM v >>= f = SetM $ \s -> do { x <- v s; runSetM (f x) s }
        {-# INLINE (>>=) #-}

    -- SAME
    instance Applicative (SetM s) where
        pure x = SetM $ const (return x)
        {-# INLINE pure #-}
        SetM f <*> SetM v = SetM $ \s -> f s >>= (`fmap` v s)
        -- We could also use the following definition
        --   SetM f <*> SetM v = SetM $ \s -> f s <*> v s
        -- but Applicative (ST s) instance is present only in GHC 7.2+
        {-# INLINE (<*>) #-}
    
    -- SAME
    run      :: Bounds -> (forall s. SetM s a) -> a
    run bnds act  = runST (newArray bnds False >>= runSetM act)
    
    -- SAME
    contains     :: Vertex -> SetM s Bool
    contains v    = SetM $ \ m -> readArray m v
    
    -- SAME
    include      :: Vertex -> SetM s ()
    include v     = SetM $ \ m -> writeArray m v True

    -- deep-first search implementation
    -- SAME
    dfs :: Graph -> [(Vertex,Link)] -> Forest Vertex Link (Vertex, Link)
    dfs g vs = prune (bounds g) (map (generate g) vs)

    -- finds the back links and their associated nodes. Returns a list of
    -- backlinks + link successors + links predecessors
    -- NEW
    loopLinks :: [Edge] -> [Vertex] -> [Link]
    loopLinks [] _ = []
    loopLinks nw rs = nub (bls ++ preds ++ succs) ++ blinksDup
        where
            bnds = elBounds nw
            fout = map (\(f,(_,l)) -> (f,l)) nw
            fin  = map (\(_,(t,l)) -> (t,l)) nw
            foutA = accumArray (flip(:)) [] bnds fout
            finA  = accumArray (flip(:)) [] bnds fin
            g =  buildG bnds nw
            backEs = backEdgesF g rs
            blinksDup = concatMap (findBackLinkDup finA foutA) backEs
            -- crossls =  crossLinks nw rs
            bls =  backLinks nw rs
            froms = map (\(f,_) -> f) backEs
            tos   = map (\(_,(t,_)) -> t) backEs
            preds = concatMap (finA!)  froms
            succs = concatMap (foutA!) tos

    -- NEW
    findBackLinkDup :: Eq a => Array Int [a] -> Array Int [a] -> (Int, (Int, a)) -> [a]
    findBackLinkDup fin fout (from, (to, backlink))
        | preds `intersect` succs == [] = []
        | preds `intersect` (succs `intersect` [backlink]) /= [] = [backlink,backlink]
        | otherwise = [backlink]
        where
            preds =  fin!from
            succs =  fout!to

    -- NEW
    --- misc functions
    -- gets the bounds of a list of edges
    elBounds :: [Edge] -> Bounds
    elBounds el = (minimum flatEl, maximum flatEl)
        where flatEl = foldl' (\l (f,(t,_)) -> f:t:l) [] el

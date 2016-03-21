{-
    Teak synthesiser for the Balsa language
    Copyright (C) 2007-2010 The University of Manchester

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
    School of Computer Science, The University of Manchester
    Oxford Road, MANCHESTER, M13 9PL, UK
-}

module Network (
    Network (..)
    ) where

    import ParseTree
    import NetParts
    import Show
    import State
    import Misc

    import Data.Maybe
    import Data.List (partition, foldl')
    import qualified Data.IntMap as IM

    data Network = Network {
        networkLinkCount :: Int,
        networkCompCount :: Int,
        networkLinks :: IM.IntMap NetworkLink,
        networkLinkUsage :: IM.IntMap NetworkLinkUsage,
        networkComps :: IM.IntMap NetworkComp,
        networkAccesses :: [Access],
        networkProperties :: [NetworkProperty]
        }

    instance NetworkIF Network where
        nwNewNetwork = Network 0 0 IM.empty IM.empty IM.empty [] []
        nwGetAccesses = NetworkMonad $ state op
            where op nw = (networkAccesses nw, nw)
        nwGetCompBounds = NetworkMonad $ state op
            where op nw = ((1, networkCompCount nw), nw)
        nwGetLinkBounds = NetworkMonad $ state op
            where op nw = ((1, networkLinkCount nw), nw)
        nwAddLink link = NetworkMonad $ state op
            where op nw = (link', nw {
                networkLinks = links',
                networkLinkUsage = usage',
                networkLinkCount = count })
                    where
                      count = networkLinkCount nw + 1
                      link' = link { nwLinkIndex = count }

                      links' = IM.insert count link' (networkLinks nw)
                      usage' = IM.insert count nwNoLinkUsage (networkLinkUsage nw)
        nwAddComp comp = NetworkMonad $ state op
            where op nw = (comp', nw {
                networkComps = comps',
                networkLinkUsage = usage',
                networkCompCount = count })
                    where
                      count = networkCompCount nw + 1
                      comp' = comp { nwCompIndex = count }

                      comps' = IM.insert count comp' (networkComps nw)
                      usage' = addCompUsage (networkLinkUsage nw) comp'
        nwLinkIsValid (Link i) = NetworkMonad $ state op
            where op nw = (isJust link, nw)
                    where link = IM.lookup i (networkLinks nw)
        nwGetLink (Link i) = NetworkMonad $ state op
            where op nw
                    | isJust link = (fromJust link, nw)
                    | otherwise = error $ "nwGetLink: can't find link at index " ++ show i
                        ++ " accesses: " ++ show (networkAccesses nw)
                    where link = IM.lookup i (networkLinks nw)
        nwGetComp (Comp i) = NetworkMonad $ state op
            where op nw = (IM.lookup i (networkComps nw), nw)
        nwGetLinkUsage sense link = NetworkMonad $ state op
            where op nw = (if isJust linkUsage && conn /= NoConn then Just conn else Nothing, nw)
                    where
                        linkUsage = IM.lookup (nwLink link) (networkLinkUsage nw)
                        Just (NetworkLinkUsage passive active) = linkUsage
                        conn = case sense of
                            Passive -> passive
                            Active -> active
        nwRemoveCompRef (Comp compNo) = NetworkMonad $ state op
            where op nw = ((), nw {
                networkComps = comps',
                networkLinkUsage = usage' })
                    where
                     comps = networkComps nw
                     usage = networkLinkUsage nw
                     comp = IM.lookup compNo (networkComps nw)
                     (comps', usage')
                         | isJust comp = (IM.delete compNo comps, unlinkCompUsage usage (fromJust comp))
                         | otherwise = (comps, usage)
        nwUpdateComp comp = NetworkMonad $ state op
            where op nw = ((), nw {
                networkComps = comps',
                networkLinkUsage = addCompUsage usage' comp})
                    where
                      comps = networkComps nw
                      usage = networkLinkUsage nw
                      compNo = nwCompIndex comp

                      oldComp = IM.lookup compNo (networkComps nw)
                      comps' = IM.insert compNo comp comps

                      usage'
                          | isJust oldComp = unlinkCompUsage usage (fromJust oldComp)
                          | otherwise = usage
        nwUpdateLink link = NetworkMonad $ state op
            where op nw = ((), nw { networkLinks = links' })
                    where
                     links = networkLinks nw
                     linkNo = nwLinkIndex link

                     links' = IM.insert linkNo link links
        nwRemoveLinkRef link = do
            unused <- nwLinkIsUnused link
            if not unused
                then error $ "netMod: tried to remove in-use link: " ++ show link
                else (NetworkMonad $ state reallyRemoveLink)
            where reallyRemoveLink nw = (oldLink, nw {
                networkLinks = IM.delete (nwLink link) (networkLinks nw),
                networkLinkUsage = IM.delete (nwLink link) (networkLinkUsage nw) })
                    where oldLink = IM.lookup (nwLink link) (networkLinks nw)
        nwSetProperties props = NetworkMonad $ state op
            where op nw = ((), nw { networkProperties = props })
        nwGetProperties = NetworkMonad $ state op
            where op nw = (networkProperties nw, nw)
        nwAddAccess access = NetworkMonad $ state op
            where op nw = ((), nw {
                networkAccesses = nwCombineAccesses accesses [access],
                networkLinkUsage = foldl' addUsage (networkLinkUsage nw) $ nwAccessLinkUsage bodyCount access })
                    where
                      accesses = networkAccesses nw
                      bodyCount = maybe 0 (length . accessBodys) $ nwFindAccess accesses $ accessRef access
        nwRemoveAccess ref = NetworkMonad $ state op
            where op nw = (listToMaybe thisAccesses, nw {
                networkAccesses = accesses',
                networkLinkUsage = usage' })
                    where
                      (thisAccesses, accesses') = partition (nwMatchAccess ref) $ networkAccesses nw
                      usage' = foldl' unlinkAccessUsage (networkLinkUsage nw) thisAccesses
                      unlinkAccessUsage usage access = foldl' unlinkUsage usage $ nwAccessLinkUsage 0 access

    addUsage :: IM.IntMap NetworkLinkUsage -> (Int, NetworkLinkUsage) -> IM.IntMap NetworkLinkUsage
    addUsage usage (linkNo, newLinkUsage) = IM.insert linkNo linkUsage' usage
        where
            oldLinkUsage = fromMaybe nwNoLinkUsage (IM.lookup linkNo usage)
            linkUsage' = addUsagePair (show linkNo) oldLinkUsage newLinkUsage

    unlinkUsage :: IM.IntMap NetworkLinkUsage -> (Int, NetworkLinkUsage) -> IM.IntMap NetworkLinkUsage
    unlinkUsage usage (linkNo, rmLinkUsage)
        | linkUsage' == nwNoLinkUsage = IM.delete linkNo usage
        | otherwise = IM.insert linkNo linkUsage' usage
        where
            oldLinkUsage = fromMaybe nwNoLinkUsage (IM.lookup linkNo usage)
            linkUsage' = removeUsagePair oldLinkUsage rmLinkUsage

    addCompUsage :: IM.IntMap NetworkLinkUsage -> NetworkComp -> IM.IntMap NetworkLinkUsage
    addCompUsage usage comp = foldl' addUsage usage $ nwCompLinkUsage comp

    unlinkCompUsage :: IM.IntMap NetworkLinkUsage -> NetworkComp -> IM.IntMap NetworkLinkUsage
    unlinkCompUsage usage comp = foldl' unlinkUsage usage $ nwCompLinkUsage comp

    -- Show/Read below

    instance ShowTab Network where
        showsPrecTab = showNetwork True
        showListTab = showBlockList

    instance Show Network where
        showsPrec prec = showsPrecTab prec 0

    showNetwork :: Bool -> Int -> Int -> Network -> ShowS
    showNetwork withUsage prec tabs network = showParen (prec > applyPrecedence) (showBody network)
        where
            showBody (Network linkCount compCount links usage comps accesses properties) =
                showString "Network" .
                tabbedNL tabs . showChar '{' .
                tabbedNL tabs' . showString "networkLinkCount = " . shows linkCount . showChar ',' .
                tabbedNL tabs' . showString "networkCompCount = " . shows compCount . showChar ',' .
                tabbedNL tabs' . showString "networkLinks = " .
                showListTab tabs' (IM.elems links) .
                showChar ',' .
                (if withUsage
                    then tabbedNL tabs' . showString "networkLinkUsage = " .
                        showListTab tabs' (filter ((/= nwNoLinkUsage) . snd) $ IM.assocs usage) . showString ","
                    else id) .
                tabbedNL tabs' . showString "networkComps = " .
                showListTab tabs' (IM.elems comps) .
                showChar ',' .
                tabbedNL tabs' . showString "networkAccesses = " .
                showListTab tabs' accesses .
                showChar ',' .
                tabbedNL tabs' . showString "networkProperties = " .
                showListTab tabs' properties .
                tabbedNL tabs . showChar '}'

            tabs' = tabs + 1

    readNetwork :: ReadS Network
    readNetwork str = maybeToList $ do
        (headToken, rest) <- maybeLex str
        case headToken of
            "Network" -> do
                ((hasUsage, network), rest2) <- readFields fields (False, nwNewNetwork) rest
                let
                    usage = IM.fromAscList $ makeLinkUsage
                        (IM.elems (networkComps network)) (networkAccesses network)
                if hasUsage
                    then return (network, rest2)
                    else return (network { networkLinkUsage = usage }, rest2)
                where fields = [
                        ReadField "networkLinkCount" readsC (\(u, o) f -> (u, o { networkLinkCount = f })),
                        ReadField "networkCompCount" readsC (\(u, o) f -> (u, o { networkCompCount = f })),
                        ReadField "networkLinks" readListC (\(u, o) f -> (u, o { networkLinks = makeLinkMap f })),
                        ReadField "networkComps" readListC (\(u, o) f -> (u, o { networkComps = makeCompMap f })),
                        ReadField "networkLinkUsage" readListC
                            (\(_, o) f -> (True, o { networkLinkUsage = IM.fromAscList f })),
                        ReadField "networkAccesses" readListC (\(u, o) f -> (u, o { networkAccesses = f })),
                        ReadField "networkProperties" readListC (\(u, o) f -> (u, o { networkProperties = f })) ]
            _ -> Nothing
            where
                makeCompMap comps = IM.fromAscList $ map compToAssoc comps
                    where compToAssoc comp = (nwCompIndex comp, comp)

                makeLinkMap links = IM.fromAscList $ map linkToAssoc links
                    where linkToAssoc link = (nwLinkIndex link, link)

    instance Read Network where
        readsPrec _ = readParen False readNetwork

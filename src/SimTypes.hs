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

module SimTypes (
    DeclState (..),
    SimState (..),
    SimRef (..),
    SimContext,
    SimUpdates,
    Repeat (..),
    Handshake (..),
    SpecialSimValue (..),
    chanRef,
    chanPop,
    chanProbe,
    chanPush,
    chanBeenRead,
    SimValue (..),
    SimFlow,
    simFlowGet,
    simFlowNewUid,
    simFlowIo,
    simFlowSet,
    simFlowLocalContext,
    simFlowConstContext,
    simFlowGetContext,
    simFlowCallContext,
    svToInt,
    svToString,
    svToUid,
    intToSv,
    stringToSv,
    specialToSv,
    uidToSv,
    progressSeq,
    progressPar,
    numberToString,
    simExtractBitfield,
    simInsertBitfield,
    simAppendValues,
    offsetSimValue,
    varRead,
    varWrite,
    showSimValue
    ) where

    import Bits
    import Context
    import ParseTree
    import Type
    import State
    import Misc

    import Data.Array
    import Control.Monad
    import Data.Maybe
    import Data.List
    import System.IO
    import Control.Monad.Trans
    import Data.Bits

    data DeclState = NoDeclState
        | ChanState Int [SimValue]
        | VarState SimValue
        | UidState Int
        | TimeState Integer
        | ChanCapacityState Int
        | RunState Bool
        | DefinesState [(String, String)]
        | FileState Handle
        | MemoryState [(Integer, SimValue)]
        deriving (Show, Eq)

    data SimState = AtomicState
        | SeqState Int [SimState]
        | ParState [Repeat] [SimState]
        | CaseState (Maybe Int) [SimState]
        | ChanOutputState (Maybe SimValue) Handshake
        | ContextState SimContext SimState
        | InstanceCallState [(SimRef, SimRef)] SimState Cmd
        | WhileState Int SimState SimState
        | EncInputState Handshake SimContext SimState
        | SelectState (Maybe Int) [SimState]
        | ArbState Int (Maybe Int) [SimState]
        deriving Show

    instance Eq SimState where
        AtomicState == AtomicState = True -- FIXME, need to think about this
        SeqState seqL subL == SeqState seqR subR = seqL == seqR && subL == subR
        ParState parL subL == ParState parR subR = parL == parR && subL == subR
        CaseState choiceL subL == CaseState choiceR subR = choiceL == choiceR && subL == subR
        ChanOutputState valueL hsL == ChanOutputState valueR hsR = valueL == valueR && hsL == hsR
        ContextState contextL stateL == ContextState contextR stateR = contextL == contextR && stateL == stateR
        InstanceCallState refL stateL _ == InstanceCallState refR stateR _ = refL == refR && stateL == stateR
        WhileState phaseL cmd1L cmd2L == WhileState phaseR cmd1R cmd2R = phaseL == phaseR && cmd1L == cmd1R &&
            cmd2L == cmd2R
        EncInputState hsL contextL stateL == EncInputState hsR contextR stateR = hsL == hsR && contextL == contextR &&
            stateL == stateR
        SelectState choiceL subL == SelectState choiceR subR = choiceL == choiceR && subL == subR
        ArbState phaseL choiceL subL == ArbState phaseR choiceR subR = phaseL == phaseR && choiceL == choiceR &&
            subL == subR
        _ == _ = False

    data SimRef =
          TimeRef
        | RunRef
        | ChanCapacityRef
        | DefinesRef
        | UidCountRef -- simulation unique id. for global objects
        | UidRef Int
        -- Important for ordering that SimRef is last (most local)
        | SimRef Int
        | LogFileRef
        deriving (Show, Eq, Ord)

    type SimContext = [(SimRef, DeclState)]
    type SimUpdates = [(SimRef, DeclState)]

    data Repeat = Done | Redo
        deriving (Show, Eq)

    data Handshake = BetweenHandshakes | InHandshake
        deriving (Show, Eq)

    data SpecialSimValue = StringSimValue String
        | UidSimValue Int
        deriving (Show, Eq, Ord)

    data SimValue = NoSimValue
        | SimValue Integer [(Int, SpecialSimValue)]
        deriving (Show, Eq)

    type SimFlow = StateT [SimContext] IO

    simFlowGet :: SimRef -> SimFlow DeclState
    simFlowGet ref = StateT op where
        op cs
            | isJust maybeValue = return (fromJust maybeValue, cs)
            | otherwise = error $ "Can't find ref: " ++ show ref ++ " " ++ show (map (map fst) cs)
            where
                maybeValue = body cs
                    where
                        body [] = Nothing
                        body (c:cs)
                            | isJust found = found
                            | otherwise = body cs
                            where found = lookup ref c

    simFlowNewGlobal :: SimRef -> DeclState -> SimFlow ()
    simFlowNewGlobal ref value = StateT op where
        op cs = return ((), pref ++ [(ref, value) : global])
            where
                csCount = length cs
                pref = take (csCount - 1) cs
                global = last cs

    simFlowNewUid :: DeclState -> SimFlow Int
    simFlowNewUid value = do
        UidState nextUid <- simFlowGet UidCountRef
        simFlowSet UidCountRef (UidState (nextUid + 1))
        simFlowNewGlobal (UidRef nextUid) value
        return nextUid

    simFlowIo :: IO a -> SimFlow a
    simFlowIo = lift

    simFlowSet :: SimRef -> DeclState -> SimFlow ()
    simFlowSet ref value = StateT op where
        op cs = do
            let cs' = updateCs cs
            return ((), cs')
            where
                updateCs [] = error $ "simFlowSet: unmatched ref " ++ show ref ++ " in  " ++ show cs
                updateCs (c:cs)
                    | isJust i = c':cs
                    | otherwise = c : updateCs cs
                    where
                        i = findIndex ((== ref) . fst) c
                        c' = replaceAt c (fromJust i) (ref, value)

    simFlowLocalContext :: SimContext -> SimFlow a -> SimFlow (SimContext, a)
    simFlowLocalContext ctx flow = StateT op
        where op cs = do
                (ret, ctx':cs') <- runStateT flow (ctx:cs)
                return ((ctx', ret), cs')

    simFlowConstContext :: SimContext -> SimFlow a -> SimFlow a
    simFlowConstContext ctx flow = StateT op
        where op cs = do
                (ret, _:cs') <- runStateT flow (ctx:cs)
                return (ret, cs')

    simFlowGetContext :: SimFlow [SimContext]
    simFlowGetContext = StateT op
        where op cs = return (cs, cs)

    simFlowCallContext :: [(SimRef, SimRef)] -> SimFlow a -> SimFlow a
    simFlowCallContext ctx flow = do
        let froms = map fst ctx
        toStates <- mapM (simFlowGet . snd) ctx

        (ctx', ret) <- simFlowLocalContext (zip froms toStates) flow

        mapM_ (uncurry simFlowSet) $ map mapUpdate ctx'

        return ret
        where
            mapUpdate (from, newState) = (to, newState)
                where Just (_, to) = find ((== from) . fst) ctx

    svToInt :: SimValue -> Integer
    svToInt (SimValue int []) = int
    svToInt other = error $ "??? " ++ show other

    svToSpecial :: SimValue -> SpecialSimValue
    svToSpecial (SimValue 0 [(0, special)]) = special
    svToSpecial value = error $ "svToSpecial: must be exactly one special `" ++ show value ++ "'"

    svToString :: SimValue -> String
    svToString (SimValue 0 [(0, StringSimValue str)]) = str
    svToString value = "(-- Invalid string: " ++ show value ++ " --)"

    svToUid :: SimValue -> SimRef
    svToUid sv = UidRef uid
        where UidSimValue uid = svToSpecial sv

    intToSv :: Integer -> SimValue
    intToSv int = SimValue int []

    stringToSv :: String -> SimValue
    stringToSv str = specialToSv $ StringSimValue str

    specialToSv :: SpecialSimValue -> SimValue
    specialToSv special = SimValue 0 [(0, special)]

    uidToSv :: Int -> SimValue
    uidToSv uid = specialToSv $ UidSimValue uid

    progressSeq :: (SimState -> Cmd -> SimFlow (Repeat, SimState)) ->
        Int -> [SimState] -> [Cmd] -> SimFlow (Int, [SimState])
    progressSeq simCmd from states cmds = body 0 [] $ zip states cmds
        where
            body count ret [] = return (count, reverse ret)
            body count ret ((state,cmd):scs)
                | count < from = body (count + 1) (state:ret) scs
                | otherwise = do
                    (cmdDone, cmdState) <- simCmd state cmd
                    case cmdDone of
                        Done -> body (count + 1) (cmdState:ret) scs
                        Redo -> return (count, reverse ret ++ (cmdState : map fst scs))

    progressPar :: (SimState -> Cmd -> SimFlow (Repeat, SimState)) ->
        [Repeat] -> [SimState] -> [Cmd] -> SimFlow ([Repeat], [SimState])
    progressPar simCmd dones states cmds = mapAndUnzipM body $ zip3 dones states cmds
        where
            body (Done, state, _) = return (Done, state)
            body (Redo, state, cmd) = simCmd state cmd

    numberToString :: Type -> Integer -> Integer -> Integer -> Bool -> String
    numberToString _ 0 _ _ False = "0"
    numberToString typ val radix underscoreSpacing showLeadingZeroes =
        if val' < 0 then '-':unsignedString else unsignedString
        where
            signed = typeIsSigned [] typ
            val' = if signed then recoverSign width val else val
            negative = val' < 0
            pos = if negative then - val' else val'
            width = widthOfType [] typ
            unsignedWidth = case (signed, width) of
                (True, 1) -> 1
                (True, n) -> n - 1
                (False, n) -> n
            maxUnsigned = bit unsignedWidth - 1

            unsignedString = intToString maxUnsigned pos False 1 ""

            -- intToString max val usCount ret
            intToString :: Integer -> Integer -> Bool -> Integer -> String -> String
            intToString 0 _ _ _ ret = ret
            intToString _ 0 _ _ ret
                | not showLeadingZeroes = ret
            intToString max val usNext usCount ret
                | usCount == underscoreSpacing = next True 1 $ ret'
                | otherwise = next False (usCount + 1) $ ret'
                where
                    digit = val `mod` radix
                    digitChar = digitChars ! fromEnum digit
                    val' = val `div` radix
                    max' = max `div` radix
                    next = intToString max' val'
                    ret' = digitChar : if usNext then '_':ret else ret

    showSimValue :: Bool -> Type -> SimValue -> ShowS
    showSimValue bareString typ val = body typ
        where
            body (Bits {}) = (numberToString typ int 10 0 False ++)
            body (SignedBits {}) = (numberToString typ int 10 0 False ++)
            body (BuiltinType "String")
                | bareString = showString str
                | otherwise = showString $ show str
            body (StructArrayType (Interval arrayRange _) elemType) = showAggr (replicate elemCount elemType) ranges
                where
                    elemCount = rangeSize arrayRange
                    elemWidth = widthOfType [] elemType
                    ranges = map indexToRange [0..elemCount - 1]
                    indexToRange i = (i * elemWidth) +: elemWidth
            body (StructRecordType _ elems _) = showAggr types ranges
                where
                    types = map recordElemType elems
                    recordElemType (RecordElem _ _ typ) = typ
                    (_, ranges) = mapAccumL makeRange 0 types
                    makeRange offset typ = (offset + typeWidth, offset +: typeWidth)
                        where typeWidth = widthOfType [] typ
            body (StructEnumType name elems _)
                | isJust found = showString $ simpleBindingName (fromJust found)
                | otherwise = showString $ show int ++ "'" ++ name
                where
                    found = find ((== int) . simpleBindingValue) elems
            body _ = showString "(-- ? " . shows val . showString " : " . shows typ . showString " --)"

            int = svToInt val
            str = svToString val
            showAggr types ranges = showChar '{' . showElems types ranges val . showChar '}'

    showElems :: [Type] -> [Slice Int] -> SimValue -> ShowS
    showElems _ [] _ = id
    showElems [typ] [range] val = showSimValue False typ (simExtractBitfield range val)
    showElems (typ:typs) (range:ranges) val = showSimValue False typ (simExtractBitfield range val)
        . showChar ',' . showElems typs ranges val
    showElems _ _ _ = error "showElems: mismatched list lengths"

    simExtractBitfield :: Slice Int -> SimValue -> SimValue
    simExtractBitfield slice (SimValue int specialValues) = SimValue int' specialValues'
        where
            int' = extractBitfield slice int
            specialValues' = map decSpecialIndex $ filter (sliceInRange slice . fst) specialValues

            decSpecialIndex (index, special) = (index - sliceLow slice, special)
    simExtractBitfield _ _ = error "simExtractBitField: sim values must not be NoSimValue"

    -- simInsertBitfield : insert second value into first at the given bitfield.  Note that the
    --    inserted (second) value *must* be small enough to fit in the bitfield
    simInsertBitfield :: Slice Int -> SimValue -> SimValue -> SimValue
    simInsertBitfield slice (SimValue value valueSpecials) (SimValue bitfield bitfieldSpecials) = result
        where
            result = SimValue (insertBitfield value slice bitfield) resultSpecials
            resultSpecials = filter (not . sliceInRange slice . fst) valueSpecials
                ++ map incSpecialIndex bitfieldSpecials

            incSpecialIndex (index, special) = (index + sliceLow slice, special)
    simInsertBitfield _ _ _ = error "simInsertBitField: sim values must not be NoSimValue"

    simAppendValues :: [Int] -> [SimValue] -> SimValue
    simAppendValues widths values = snd $ foldl' append (0, SimValue 0 []) $ zip widths values
        where
            append (i, ret) (0, _) = (i, ret)
            append (i, ret) (width, value) = (i + width, simOrValues ret $ offsetSimValue i value)

    simOrValues :: SimValue -> SimValue -> SimValue
    simOrValues NoSimValue simValue = simValue
    simOrValues simValue NoSimValue = simValue
    simOrValues (SimValue li ls) (SimValue ri rs) = SimValue (li .|. ri) (ls ++ rs)

    offsetSimValue :: Int -> SimValue -> SimValue
    offsetSimValue offset (SimValue value specials) = SimValue (value `shiftL` offset) $ map offsetSpecial specials
        where offsetSpecial (specialOffset, specialValue) = (specialOffset + offset, specialValue)
    offsetSimValue _ _ = error "offsetSimValue: arg must not be NoSimValue"

    varRead :: Ref -> SimFlow SimValue
    varRead (Ref int) = do
        VarState value <- simFlowGet (SimRef int)
        return value
    varRead _ = error "varRead: must be a simple ref"

    varWrite :: Ref -> SimValue -> SimFlow ()
    varWrite (Ref int) value = do
        simFlowSet (SimRef int) (VarState value)
    varWrite _ _ = error "varWrite: must be a simple ref"

    chanRef :: Chan -> SimRef
    chanRef (Chan _ (Ref int)) = SimRef int
    chanRef _ = error "chanRef: must be a simple Chan"

    -- chanProbe : can this chan be popped
    chanProbe :: Chan -> SimFlow (Maybe SimValue)
    chanProbe chan  = do
        state <- simFlowGet (chanRef chan)
        case state of
            ChanState _ [] -> return Nothing
            ChanState 0 [value] -> return $ Just value
            ChanState _ values -> return $ Just $ last values
            _ -> error "chanProbe: unrecognised state"

    -- chanPop : pop a value from a channel.  Returns Nothing if the channel can't currently be read
    chanPop :: Chan -> SimFlow (Maybe SimValue)
    chanPop chan = do
        maybeValue <- chanProbe chan
        if isJust maybeValue
            then do
                ChanState capacity values <- simFlowGet (chanRef chan)
                simFlowSet (chanRef chan) (ChanState capacity (init values))
                return maybeValue
            else return Nothing

    -- chanPush : try to push a value on a channel.  Returns True on succes or False if the push
    --    needs to be tried again
    chanPush :: Chan -> SimValue -> SimFlow Bool
    chanPush chan value = do
        state <- simFlowGet (chanRef chan)
        case state of
            ChanState 0 [] -> do
                simFlowSet (chanRef chan) (ChanState 0 [value])
                return True
            ChanState capacity values
                | capacity > 0 && length values < capacity -> do
                    simFlowSet (chanRef chan) (ChanState capacity (value:values))
                    return True
            _ -> return False

    -- chanBeenRead : for synchronouse channels, has a previously pushed value actually been read.
    --    for asynchronous channels, always true
    chanBeenRead :: Chan -> SimFlow Bool
    chanBeenRead chan = do
        state <- simFlowGet (chanRef chan)
        case state of
            ChanState 0 [_] -> return False
            _ -> return True

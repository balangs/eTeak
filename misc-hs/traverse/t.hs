module Main (
	main
	) where

	import Language.Haskell.Parser
	import Language.Haskell.Syntax
	import Language.Haskell.Pretty
	import Control.Monad
	import Data.List
	import Misc
	import Maybe
	import System
	import IO
	
	isHsDataDeclWithDeriving :: String -> HsDecl -> Bool
	isHsDataDeclWithDeriving derivName (HsDataDecl _ _ _ _ _ derivings) = elem (UnQual (HsIdent derivName)) derivings 
	isHsDataDeclWithDeriving _ _ = False

	isHsInstDeclForType :: HsQName -> HsDecl -> Bool
	isHsInstDeclForType typ1 (HsInstDecl _ _ typ2 _ _) = typ1 == typ2
	isHsInstDeclForType _ _ = False
	
	hsInstDeclType :: HsDecl -> Maybe HsType
	hsInstDeclType (HsInstDecl _ _ _ [typ] _) = Just typ
	hsInstDeclType _ = Nothing

	hsDataDeclName :: HsDecl -> String
	hsDataDeclName decl = unpickName name
		where HsDataDecl _ _ name _ _ _ = decl
	
	unpickName :: HsName -> String
	unpickName (HsIdent name) = name
	unpickName name = error $ show name

	type DataCons = [(String, [(String, HsType)])]
	
	hsDataDeclCons :: HsDecl -> DataCons
	hsDataDeclCons decl = concat $ snd $ mapAccumL unpickCons argNames cons
		where
			HsDataDecl _ _ _ _ cons _ = decl
	
			unpickMemberType (HsUnBangedTy typ) = typ
			unpickMemberType (HsBangedTy typ) = typ
	
			argNames = map (("arg" ++) . show) [(1::Int)..]
	
			unpickCons anonNames (HsConDecl _ (HsIdent name) members) = (otherNames,
				[(name, zip theseNames $ map unpickMemberType members)])
				where (theseNames, otherNames) = splitAt (length members) anonNames
			unpickCons anonNames (HsRecDecl _ (HsIdent name) members) = (anonNames,
				[(name, concatMap unpickMember members)])
				where unpickMember (names, typ) = zip (map unpickName names) (repeat (unpickMemberType typ))
			unpickCons anonNames _ = (anonNames, [])
	
	data VisitType = VisitNode | VisitTerminal
		deriving (Show, Eq)

	visitTypeChar :: VisitType -> Char
	visitTypeChar VisitNode = 'N'
	visitTypeChar VisitTerminal = 'V'
	
	noLoc :: SrcLoc
	noLoc = SrcLoc "<unknown>" 1 1
	
	bind :: String -> HsExp -> HsDecl
	bind name value = HsPatBind noLoc (HsPVar (HsIdent name)) (HsUnGuardedRhs value) []

	dummyRhs :: HsRhs
	dummyRhs = HsUnGuardedRhs (HsVar (UnQual (HsIdent "dummy")))

	typeVisitType :: HsType -> VisitType
	typeVisitType _ = VisitNode
	
	makeDataInstance :: String -> HsDecl -> (HsDecl, [[VisitType]])
	makeDataInstance className decl = (HsInstDecl noLoc [] classHsName
		[HsTyCon (UnQual (HsIdent typeName))] decls, nub visitPatterns)
		where
			decls = [pre, mid, post, descend]
	
			pre = bind "pre" (HsVar (UnQual (HsIdent ("pre" ++ typeName))))
			mid = bind "mid" (HsVar (UnQual (HsIdent ("mid" ++ typeName))))
			post = bind "post" (HsVar (UnQual (HsIdent ("post" ++ typeName))))
			descend = HsFunBind matches

			(matches, visitPatterns) = unzip $ map descendMatch cons
	
			descendMatch (consName, args) = (match, visitPattern)
				where
					match = HsMatch noLoc (HsIdent "descend")
						[HsPVar (HsIdent "apply"), HsPParen (HsPApp (UnQual (HsIdent consName))
							(map (HsPVar . HsIdent) argNames))]
						(HsUnGuardedRhs
							(foldl1' HsApp (descendFunc:(map (HsVar . UnQual . HsIdent) ("apply":consName:argNames)))))
						[]

					(argNames, argTypes) = unzip args
					descendFunc = HsVar $ UnQual $ HsIdent $ "descend" ++ map visitTypeChar visitPattern
					visitPattern = map typeVisitType argTypes
	
			classHsName = UnQual $ HsIdent className
			typeName = hsDataDeclName decl
			cons = hsDataDeclCons decl

	plainTextTypeName :: HsType -> String
	plainTextTypeName (HsTyApp (HsTyCon (Special HsListCon)) typ) = "ListOf" ++ prettyPrint typ
	plainTextTypeName typ = prettyPrint typ
	
	makeTerminalInstance :: String -> HsType -> HsDecl
	makeTerminalInstance className typ = HsInstDecl noLoc [] classHsName [typ] decls
		where
			decls = [post]

			post = bind "post" (HsVar (UnQual (HsIdent ("post" ++ plainTextTypeName typ))))
	
			classHsName = UnQual $ HsIdent className

	{-
		Allow number of `passes' to be specified and which arguments are processed on which
		pass.  Literal/non-literal distinction doesn't necessarily need to be made.

		So: BinExpr pos typ op left right is: NNNNV -> VVVVN, give as 11112?
	-}

	makeDescend :: String -> [VisitType] -> [HsDecl]
	makeDescend className visits = [typeSig, func]
		where
			argNames = map (("arg" ++) . show) [1..length visits]
			typeSig = HsTypeSig noLoc
				[HsIdent descendName]
				(HsQualType
					(map makeContext argNames)
					(foldr1 HsTyFun ((applyType:consType:(map (HsTyVar . HsIdent) argNames)) ++ [descendType])))

			makeContext argName = (UnQual (HsIdent className), [HsTyVar $ HsIdent argName])

			applyType = foldl1' HsTyApp [HsTyCon (UnQual (HsIdent "Apply")),
				stringToTyVar "context",
				stringToTyVar "accum",
				stringToTyVar "result"]

			consType = foldr1 HsTyFun $ map (HsTyVar . HsIdent) (argNames ++ ["node"])

			descendType = foldl1' HsTyApp [HsTyCon (UnQual (HsIdent "Descend")),
				stringToTyVar "context",
				stringToTyVar "accum",
				stringToTyVar "result",
				stringToTyVar "node"]

			func = HsFunBind [HsMatch noLoc (HsIdent descendName)
				[HsPVar (HsIdent "apply"), HsPVar (HsIdent "cons")]
				(HsUnGuardedRhs (HsInfixApp
					(applyFunc "descend" [stringToVar "apply", stringToVar "cons"])
					(HsQVarOp (UnQual (HsSymbol "$")))
					(composeApps
						(map (\argName -> applyFunc "stepD" [stringToVar "apply", stringToVar argName])
						(reverse argNames)))
					))
				[]
				]

			stringToTyVar = HsTyVar . HsIdent
			stringToVar = HsVar . UnQual . HsIdent

			composeApps apps = foldl1' (\l r -> HsInfixApp l (HsQVarOp (UnQual (HsSymbol "."))) r) apps

			applyFunc func args = foldl1' HsApp $ (stringToVar func):args

			descendName = "descend" ++ map visitTypeChar visits
	
	main :: IO ()
	main = do
		args <- getArgs
		progName <- getProgName
		(className, fileName) <- case args of
			[className, fileName] -> return (className, fileName)
			_ -> do
				hPutStrLn stderr $ "usage: " ++ progName ++ " <class-name> <file-name> "
				return ("MyTraverse", "test.hs")

		contents <- readFile fileName
		case parseModule contents of
			ParseOk m -> do
				let
					HsModule _loc _moduleName _exports _imports decls = m
	
					dataDecls = filter (isHsDataDeclWithDeriving className) decls
					instDecls = filter (isHsInstDeclForType (UnQual (HsIdent className))) decls

					terminalTypes = mapMaybe hsInstDeclType instDecls

					terminalInstances = map (makeTerminalInstance className) terminalTypes
					(dataInstances, visitPatternss) = unzip $ map (makeDataInstance className) dataDecls
					visitPatterns = nub $ concat visitPatternss

					descends = concatMap (makeDescend className) visitPatterns
	
				putStrLn $ "{- Terminals: " ++ joinWith ", " (map prettyPrint terminalTypes) ++ " -}"
				putStrLn $ "{- VisitPatterns: " ++ joinWith ", " (map (map visitTypeChar) visitPatterns) ++ " -}"

				-- Need descend/etc. functions, need Apply type, need exports, need imports

				putStrLn $ prettyPrint $ HsModule noLoc (Module className)
					Nothing [] (terminalInstances ++ dataInstances ++ descends)
			ret -> print ret
	


{-# Language DeriveFunctor #-}

module Verify where
import AST
import Lexer
import Scope

import Control.Applicative


data Check x = Check x | Fail String deriving (Show, Functor)

instance Applicative Check where
	pure = Check
	Check f <*> Check x = Check (f x)
	Fail msg <*> _ = Fail msg
	_ <*> Fail msg = Fail msg
instance Monad Check where
	return = pure
	Check x >>= f = f x
	Fail msg >>= _ = Fail msg

reject :: String -> Check any
reject = Fail

assert :: Maybe x -> String -> Check x
assert Nothing msg = reject msg
assert (Just x) _ = return x

initialScope :: Scope
initialScope = Scope
	{ scopeVariableType = []
	, scopeClassDefinitions = []
	, scopeTypeDefinitions = [concrete "Number", concrete "String", concrete "Bool"]
	, scopeFrozenStack = []
	, scopeInstances = []
	}
	where
	concrete name = (name, ScopeTypeDefinitionAbstract Concrete)

checkType :: Scope -> Type -> Check Type
checkType = undefined

checkExpectedKind :: Scope -> Type -> Kind -> Check ()
checkExpectedKind = undefined

checkConstraintSatisfaction :: Scope -> Type -> [String] -> Check ()
checkConstraintSatisfaction = undefined 

checkClassDefinition :: Scope -> (TypeHeader, Token, Token, [ClassStatement]) -> Check ModuleDeclaration
checkClassDefinition = undefined

checkInstanceStatement :: Scope -> Type -> InstanceStatement -> Check InstanceStatement
checkInstanceStatement = undefined

checkClassInstance :: Scope -> (Type, Token, [InstanceStatement]) -> Check ModuleDeclaration
checkClassInstance scope (argument, className, body) = case classDefinitionOf scope (contents className) of
	Nothing -> reject $ "cannot create instance for class `" ++ contents className ++ "` because it has not been defined"
	Just (ScopeClassDefinition argumentName argumentKind argumentConstraints classBody) -> do
		argument' <- checkType scope argument -- argument must be well-formed
		checkDesiredForm argument' -- argument must have desired form
		checkExpectedKind scope argument' argumentKind -- argument must have desired type
		checkConstraintSatisfaction scope argument' argumentConstraints -- argument must satisfy conditions
		case [name | (name, _) <- classBody, not $ name `elem` map nameOfItem body] of
			[] -> return ()
			missing -> reject $ "an instance declaration for `" ++ contents className ++ "` must include all of the following definitions:" ++ concat (map (" "++) missing)
		let innerScope = 
		-- In addition, it must consist exclusively of a typename, followed by some number of distinct free variables, possibly each with class constraints.
		body' <- mapM (checkInstanceStatement scope undefined) body -- TODO: figure out the expected type for each, based on class definition
		return $ ClassInstance argument' className body'
	where
	nameOfItem :: InstanceStatement -> String
	nameOfItem (ClassImplement name _) = contents name
	checkDesiredForm :: Type -> Check ()
	checkDesiredForm (Type (TypeHeader free _) term) = go [] term
		where
		go _ (TName left)
			|contents left `elem` map (contents . fst) free = reject $ "the identifying (leftmost) term in an instance argument must be the concrete variable of interest"
			|otherwise = return () -- fine, we're done
		go sofar (left `TApply` TName arg)
			|contents arg `elem` sofar = reject $ "the free variable `" ++ contents arg ++ "` appeared twice in the type of the instance parameter for `" ++ show argument ++ "` for class `" ++ contents className ++ "`"
			|otherwise = go (contents arg : sofar) left
		go _ (_ `TApply` right) = reject $ "the argument `" ++ show right ++ "` is not allowed for an argument in a type instance for class `" ++ contents className ++ "` because it is not a simple free variable."

-- note: do not declare
checkVariableDeclaration :: Scope -> Token -> Type -> Maybe Expression -> Check LetDeclaration
checkVariableDeclaration = undefined

checkLetDeclaration :: Scope -> LetDeclaration -> Check LetDeclaration
checkLetDeclaration scope (LetVarDeclare var t e) = checkVariableDeclaration scope var t (Just e)

forwardDeclareLetDeclaration :: Scope -> LetDeclaration -> Check Scope
forwardDeclareLetDeclaration scope (LetVarDeclare var t _) = do
	case variableTypeOf scope (contents var) of
		Just _ -> reject $ "variable `" ++ contents var ++ "` has already been declared"
		Nothing -> return ()
	return $ freezeVariable (declareVariable scope (contents var) t) (contents var)

forwardDeclareModuleDeclaration :: Scope -> ModuleDeclaration -> Check Scope
forwardDeclareModuleDeclaration scope (ModuleLet dec) = forwardDeclareLetDeclaration scope dec
forwardDeclareModuleDeclaration scope (ClassDefinition header@(TypeHeader free constraints) argument className body) = do
	case classDefinitionOf scope (contents className) of
		Just _ -> reject $ "class `" ++ contents className ++ "` has already been declared"
		Nothing -> return ()
	case contents argument `elem` map (contents . fst) free of
		True -> return ()
		False -> reject $ "class `" ++ contents className ++ "` has ill-formed argument; identifier `" ++ contents argument ++ "` is not one of the free variables in " ++ show header
	kind <- assert (lookup (contents className) $ map (\(n,k) -> (contents n,k)) free) $ "argument `" ++ contents argument ++ "` to class `" ++ contents className ++ "` must be present in the header " ++ show header

	-- verify here that the argument type is discoverable from all member types
	mapM_ checkDiscoverable body

	return $ declareClass scope className argument kind supers members
	where
	supers = [ super | ClassConstraint constrained super <- constraints, contents constrained == contents argument ]
	members = map memberFromBody body
	memberFromBody (ClassDeclare n t) = (n, t)
	checkDiscoverable (ClassDeclare item itemType@(Type (TypeHeader exclude _) term))
		|contents argument `elem` map (contents . fst) exclude || notFoundIn term = reject $ "argument `" ++ contents argument ++ "` of type class definition `" ++ contents className ++ "` does not appear in the type of class body item `" ++ contents item ++ "`, namely " ++ show itemType
		|otherwise = return ()
		where
		notFoundIn (TName name) = contents name /= contents argument
		notFoundIn (left `TApply` right) = notFoundIn left && notFoundIn right


forwardDeclareModuleDeclaration scope (ClassInstance argument className _) = do
	case nameOfType argument of
		Nothing -> reject $ "cannot create instance for class `" ++ contents className ++ "` for type `" ++ show argument ++ "` because it is not a named type"
		Just argumentName -> case hasClassInstance scope (contents className) (contents argumentName) of
			True -> reject $ "instance for class `" ++ contents className ++ "` has already been declared for `" ++ contents argumentName ++ "`, " ++ show argument
			False -> return $ declareInstance scope (contents className) (contents argumentName) argument

forwardDeclareModuleDeclarations :: Scope -> [ModuleDeclaration] -> Check Scope
forwardDeclareModuleDeclarations scope [] = return scope
forwardDeclareModuleDeclarations scope (x:xs) = do
	scope' <- forwardDeclareModuleDeclaration scope x
	forwardDeclareModuleDeclarations scope' xs

checkModuleDeclaration :: Scope -> ModuleDeclaration -> Check ModuleDeclaration
checkModuleDeclaration scope (ModuleLet dec) = fmap ModuleLet $ checkLetDeclaration scope dec
checkModuleDeclaration scope (ClassDefinition header argument name body) = checkClassDefinition scope (header, argument, name, body)
checkModuleDeclaration scope (ClassInstance argument name body) = checkClassInstance scope (argument, name, body)

checkModule :: Module -> Check ()
checkModule (Module _name declarations) = do
	scope' <- forwardDeclareModuleDeclarations initialScope declarations
	mapM_ (checkModuleDeclaration scope') declarations




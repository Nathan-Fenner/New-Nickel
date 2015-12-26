
{-# Language DeriveFunctor #-}

module Verify where
import AST
import Lexer
import Scope

import Control.Applicative


data Check x = Check x | Fail [String] deriving (Show, Functor)

instance Applicative Check where
  pure = Check
  Check f <*> Check x = Check (f x)
  Fail left <*> Fail right = Fail (left ++ right)
  Fail msg <*> _ = Fail msg
  _ <*> Fail msg = Fail msg
instance Monad Check where
  return = pure
  Check x >>= f = f x
  Fail msg >>= _ = Fail msg

reject :: String -> Check any
reject x = Fail [x]

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

tryIntroduceTypeHeader :: Scope -> TypeHeader -> Check Scope
tryIntroduceTypeHeader = undefined

checkClassInstanceStatement :: Scope -> [(String, Type)] -> InstanceStatement -> Check InstanceStatement
checkClassInstanceStatement scope classBody (ClassImplement name expression) = case lookup (contents name) classBody of
  Nothing -> reject $ "the class "

checkClassInstance :: Scope -> (Type, Token, [InstanceStatement]) -> Check ModuleDeclaration
checkClassInstance scope (argument@(Type argumentHeader instanceArgumentTerm), className, instanceBody) = case classDefinitionOf scope (contents className) of
  Nothing -> reject $ "cannot create instance for class `" ++ contents className ++ "` because it has not been defined"
  Just (ScopeClassDefinition classArgumentName classArgumentKind classArgumentConstraints classBody) -> do
    argument' <- checkType scope argument -- argument must be a well-formed type
    checkDesiredForm argument' -- argument must have desired form (a name, followed by free variables)
    checkExpectedKind scope argument' classArgumentKind -- argument must have desired type
    checkConstraintSatisfaction scope argument' classArgumentConstraints -- argument must satisfy conditions
    case [name | (name, _) <- classBody, not $ name `elem` map nameOfItem instanceBody] of
      [] -> return ()
      missing -> reject $ "an instance declaration for `" ++ contents className ++ "` must include all of the following definitions (which are missing):" ++ concat (map (" "++) missing)
    -- It declares the free variables in the instance header, so in the body they're abstract.
    innerScope <- tryIntroduceTypeHeader scope argumentHeader
    -- We specialize the class's definitions for this particular instance, so that we can do typechecking.
    let specializedClassBody = map (\(n, v) -> (n, specializeType classArgumentName instanceArgumentTerm v)) classBody
    instanceBody' <- mapM (checkClassInstanceStatement innerScope specializedClassBody) instanceBody
    -- And then we're done, if all the above checks have passed.
    return $ ClassInstance argument' className instanceBody'
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
  specializeType :: String -> Term -> Type -> Type
  specializeType name with (Type header term) = Type header (go term) where
    go (TName atom)
      |contents atom == name = with
      |otherwise = TName atom
    go (left `TApply` right) = go left `TApply` go right

-- note: do not declare
checkVariableDeclaration :: Scope -> Token -> Type -> Maybe Expression -> Check LetDeclaration
checkVariableDeclaration = undefined

checkLetDeclaration :: Scope -> LetDeclaration -> Check LetDeclaration
checkLetDeclaration scope (LetVarDeclare var t e) = checkVariableDeclaration scope var t (Just e)

forwardDeclareLetDeclaration :: Scope -> LetDeclaration -> Check Scope
forwardDeclareLetDeclaration scope (LetVarDeclare var t _)
  | isIdentifierDefined scope (contents var) = reject $ "variable `" ++ contents var ++ "` cannot be declared because it was already declared at (TODO)"
  | otherwise = return $ declareFrozenVariable scope (contents var) t

forwardDeclareModuleDeclaration :: Scope -> ModuleDeclaration -> Check Scope
forwardDeclareModuleDeclaration scope (ModuleLet dec) = forwardDeclareLetDeclaration scope dec
forwardDeclareModuleDeclaration scope (ClassDefinition header@(TypeHeader free constraints) argument className body)
  |isIdentifierDefined scope (contents className) = reject $ "class `" ++ contents className ++ "` cannot be defined: `" ++ contents className ++ "` has already been declared at (TODO)`"
  |otherwise = do
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

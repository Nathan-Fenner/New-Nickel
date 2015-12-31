
{-# Language DeriveFunctor #-}

module Verify where
import ParseAST(Parsed)
import AST
import Lexer
import Scope

import Control.Applicative

data Verified = Verified

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

checkExpressionWithExpectedType :: Scope -> Type Parsed -> Expression Parsed -> Check (Expression Verified)
checkExpressionWithExpectedType = undefined

checkType :: Scope -> Type Parsed -> Check (Type Verified)
checkType = undefined

checkExpectedKind :: Scope -> Type Verified -> Kind -> Check ()
checkExpectedKind = undefined

checkConstraintSatisfaction :: Scope -> Type Verified -> [String] -> Check ()
checkConstraintSatisfaction = undefined

checkClassDefinition :: Scope -> (TypeHeader Parsed, Token, Token, [ClassStatement Parsed]) -> Check (ModuleDeclaration Verified)
checkClassDefinition scope (header, argumentName, className, body) = undefined

checkInstanceStatement :: Scope -> Type Parsed -> InstanceStatement Parsed -> Check (InstanceStatement Verified)
checkInstanceStatement scope expectedType (ClassImplement name expression) = do
  expression' <- checkExpressionWithExpectedType scope expectedType expression
  return (ClassImplement name expression')

introduceTypeHeader :: Scope -> TypeHeader Parsed -> Scope
introduceTypeHeader scope (TypeHeader free constraints) = let
  scope' = declareAbstractTypes' (map (\(n, k) -> (contents n, k)) free) scope
  scope'' = declareConstraintSatisfactions' (map (\(ClassConstraint free className) -> (contents className, free)) constraints) scope'
  -- since it's well-formed, everything is okay
  in
  scope''

checkClassInstanceStatement :: Scope -> [(String, Type Parsed)] -> InstanceStatement Parsed -> Check (InstanceStatement Verified)
checkClassInstanceStatement scope classBody (ClassImplement name expression) = case lookup (contents name) classBody of
  Nothing -> reject $ "the class has no method called `" ++ contents name ++ "`"
  Just expected -> do
    expression' <- checkExpressionWithExpectedType scope expected expression
    return $ ClassImplement name expression'

checkClassInstance :: Scope -> (Type Parsed, Token, [InstanceStatement Parsed]) -> Check (ModuleDeclaration Verified)
checkClassInstance scope (argument@(Type argumentHeader instanceArgumentTerm), className, instanceBody) = case definitionOfClass' (contents className) scope of
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
    let innerScope = introduceTypeHeader scope argumentHeader
    -- We specialize the class's definitions for this particular instance, so that we can do typechecking.
    let specializedClassBody = map (\(n, v) -> (n, specializeType classArgumentName instanceArgumentTerm v)) classBody
    instanceBody' <- mapM (checkClassInstanceStatement innerScope specializedClassBody) instanceBody
    -- And then we're done, if all the above checks have passed.
    return $ ClassInstance argument' className instanceBody'
  where
  nameOfItem :: InstanceStatement Parsed -> String
  nameOfItem (ClassImplement name _) = contents name
  checkDesiredForm :: Type Verified -> Check ()
  checkDesiredForm (Type (TypeHeader free _) term) = go [] term
    where
    go _ (TName left)
      |contents left `elem` map (contents . fst) free = reject $ "the identifying (leftmost) term in an instance argument must be the concrete variable of interest"
      |otherwise = return () -- fine, we're done
    go sofar (left `TApply` TName arg)
      |contents arg `elem` sofar = reject $ "the free variable `" ++ contents arg ++ "` appeared twice in the type of the instance parameter for `" ++ show argument ++ "` for class `" ++ contents className ++ "`"
      |otherwise = go (contents arg : sofar) left
    go _ (_ `TApply` right) = reject $ "the argument `" ++ show right ++ "` is not allowed for an argument in a type instance for class `" ++ contents className ++ "` because it is not a simple free variable."
  specializeType :: String -> Term Parsed -> Type Parsed -> Type Parsed
  specializeType name with (Type header term) = Type header (go term) where
    go (TName atom)
      |contents atom == name = with
      |otherwise = TName atom
    go (left `TApply` right) = go left `TApply` go right

checkLetDeclaration :: Scope -> LetDeclaration Parsed -> Check (LetDeclaration Verified)
checkLetDeclaration scope (LetVarDeclare var t e) = do
  t' <- checkType scope t
  e' <- checkExpressionWithExpectedType scope t e
  return $ LetVarDeclare var t' e'

forwardDeclareLetDeclaration :: Scope -> LetDeclaration Parsed -> Check Scope
forwardDeclareLetDeclaration scope (LetVarDeclare var t _)
  | isIdentifierDefined' (contents var) scope  = reject $ "variable `" ++ contents var ++ "` cannot be declared because it was already declared at (TODO)"
  | otherwise = return $ declareFrozenVariable' (contents var) t scope

forwardDeclareModuleDeclaration :: Scope -> ModuleDeclaration Parsed -> Check Scope
forwardDeclareModuleDeclaration scope (ModuleLet dec) = forwardDeclareLetDeclaration scope dec
forwardDeclareModuleDeclaration scope (ClassDefinition header@(TypeHeader free constraints) argument className body)
  |isIdentifierDefined' (contents className) scope = reject $ "class `" ++ contents className ++ "` cannot be defined: `" ++ contents className ++ "` has already been declared at (TODO)`"
  |otherwise = do
  case contents argument `elem` map (contents . fst) free of
    True -> return ()
    False -> reject $ "class `" ++ contents className ++ "` has ill-formed argument; identifier `" ++ contents argument ++ "` is not one of the free variables in " ++ show header
  kind <- assert (lookup (contents className) $ map (\(n,k) -> (contents n,k)) free) $ "argument `" ++ contents argument ++ "` to class `" ++ contents className ++ "` must be present in the header " ++ show header
  -- verify here that the argument type is discoverable from all member types
  mapM_ checkDiscoverable body -- note: also checks that body variables don't overlap anything.
  case [name | (ClassDeclare name _) <- body, length [() | (ClassDeclare name' _) <- body, contents name == contents name'] > 1 ] of
    [] -> return ()
    duplicates -> reject $ "the following items are duplicated in the class: " ++ show duplicates
  return $ declareClass' className argument kind supers members scope
  where
  supers = [ super | ClassConstraint constrained super <- constraints, contents constrained == contents argument ]
  members = map memberFromBody body
  memberFromBody (ClassDeclare n t) = (n, t)
  checkDiscoverable (ClassDeclare item itemType@(Type (TypeHeader exclude _) term))
    |isIdentifierDefined' (contents item) scope = reject $ "class `" ++ contents className ++ "` cannot define member `" ++ contents item ++ "` because that name is already in scope, declared at (TODO)"
    |contents argument `elem` map (contents . fst) exclude || notFoundIn term = reject $ "argument `" ++ contents argument ++ "` of type class definition `" ++ contents className ++ "` does not appear in the type of class body item `" ++ contents item ++ "`, namely " ++ show itemType
    |otherwise = return ()
    where
    notFoundIn (TName name) = contents name /= contents argument
    notFoundIn (left `TApply` right) = notFoundIn left && notFoundIn right
forwardDeclareModuleDeclaration scope (ClassInstance argument className _) = do
  case nameOfType argument of
    Nothing -> reject $ "cannot create instance for class `" ++ contents className ++ "` for type `" ++ show argument ++ "` because it is not a named type"
    Just argumentName -> case getClassInstance' (contents className) (contents argumentName) scope of
      Just _ -> reject $ "instance for class `" ++ contents className ++ "` has already been declared for `" ++ contents argumentName ++ "`, " ++ show argument
      Nothing -> return $ declareInstance' (contents className) (contents argumentName) argument scope

forwardDeclareModuleDeclarations :: Scope -> [ModuleDeclaration Parsed] -> Check Scope
forwardDeclareModuleDeclarations scope [] = return scope
forwardDeclareModuleDeclarations scope (x:xs) = do
  scope' <- forwardDeclareModuleDeclaration scope x
  forwardDeclareModuleDeclarations scope' xs

checkModuleDeclaration :: Scope -> ModuleDeclaration Parsed -> Check (ModuleDeclaration Verified)
checkModuleDeclaration scope (ModuleLet dec) = fmap ModuleLet $ checkLetDeclaration scope dec
checkModuleDeclaration scope (ClassDefinition header argument name body) = checkClassDefinition scope (header, argument, name, body)
checkModuleDeclaration scope (ClassInstance argument name body) = checkClassInstance scope (argument, name, body)

checkModule :: Module Parsed -> Check (Module Verified)
checkModule (Module name declarations) = do
  scope' <- forwardDeclareModuleDeclarations initialScope declarations
  declarations' <- mapM (checkModuleDeclaration scope') declarations
  return $ Module name declarations'

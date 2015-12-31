
module Scope where
import Control.Applicative

import AST
import ParseAST
import Lexer

-- Scopes carry the information used to verify the program.

data ScopeClassDefinition = ScopeClassDefinition
  { scopeClassDefinitionName :: String                  {- argument name           -}
  , scopeClassDefinitionTermKind :: Kind                {- argument term kind      -}
  , scopeClassDefinitionSupers :: [String]              {- constraints on argument -}
  , scopeClassDefinitionBody :: [(String, Type Parsed)] {- body items              -}
  }
  deriving Show

data ScopeTypeDefinition
  = ScopeTypeDefinitionAbstract Kind
  -- TODO: struct, enum
  deriving Show

data Scope = Scope
  { scopeVariableType :: [(String, Type Parsed)]
  , scopeClassDefinitions :: [(String, ScopeClassDefinition)]
  , scopeTypeDefinitions :: [(String, ScopeTypeDefinition)]
  , scopeFrozenStack :: [String]
  , scopeInstances :: [((String, String), Type Parsed)] -- ((class, typename), full argument spec)
  } deriving Show

typeOfVariable' :: String -> Scope -> Maybe (Type Parsed)
typeOfVariable' name scope = lookup name (scopeVariableType scope)

definitionOfClass' :: String -> Scope -> Maybe ScopeClassDefinition
definitionOfClass' name scope = lookup name $ scopeClassDefinitions scope

definitionOfType' :: String -> Scope -> Maybe ScopeTypeDefinition
definitionOfType' name scope = lookup name $ scopeTypeDefinitions scope

isVariableFrozen' :: String -> Scope -> Bool
isVariableFrozen' name scope = name `elem` scopeFrozenStack scope

getClassInstance' :: String -> String -> Scope -> Maybe (Type Parsed)
getClassInstance' className argumentName scope = lookup (className, argumentName) (scopeInstances scope)

isIdentifierDefined' :: String -> Scope -> Bool
isIdentifierDefined' name scope
  |name `elem` map fst (scopeVariableType scope) = True
  |name `elem` map fst (scopeClassDefinitions scope) = True
  |name `elem` map fst (scopeTypeDefinitions scope) = True
  |otherwise = False

nameOfType :: Type Parsed -> Maybe Token
nameOfType (Type (TypeHeader free _) term) = search term where
  search (TName name)
    |contents name `elem` map (contents . fst) free = Nothing -- not a name
    |otherwise = Just name
  search (left `TApply` _) = search left

declareVariable' :: String -> Type Parsed -> Scope -> Scope
declareVariable' variableName variableType scope = scope { scopeVariableType = (variableName, variableType) : scopeVariableType scope}

declareVariables' :: [(String, Type Parsed)] -> Scope -> Scope
declareVariables' [] scope = scope
declareVariables' ((n, t) : rest) scope = declareVariables' rest $ declareVariable' n t scope

freezeVariable' :: String -> Scope -> Scope
freezeVariable' name scope = scope { scopeFrozenStack = name : scopeFrozenStack scope }

declareFrozenVariable' :: String -> Type Parsed -> Scope -> Scope
declareFrozenVariable' variableName variableType scope = freezeVariable' variableName $ declareVariable' variableName variableType scope

declareAbstractType' :: (String, Kind) -> Scope -> Scope
declareAbstractType' (name, kind) scope = scope { scopeTypeDefinitions = (name, ScopeTypeDefinitionAbstract kind) : scopeTypeDefinitions scope }

declareAbstractTypes' :: [(String, Kind)] -> Scope -> Scope
declareAbstractTypes' [] scope = scope
declareAbstractTypes' (x : xs) scope = declareAbstractTypes' xs $ declareAbstractType' x scope

declareClass' :: Token -> Token -> Kind -> [Token] -> [(Token, Type Parsed)] -> Scope -> Scope
declareClass' className argumentName argumentKind classSupers classMembers scope = declareVariables' members' $ scope { scopeClassDefinitions = (contents className, ScopeClassDefinition (contents argumentName) argumentKind (map contents classSupers) members') : scopeClassDefinitions scope }
  where
  members' = map updateMember classMembers
  updateMember (n, t) = (contents n, updateType t)
  updateType (Type header term) = Type (updateHeader header) term
  updateHeader (TypeHeader free constraints) = TypeHeader ((argumentName, argumentKind) : free) (ClassConstraint argumentName className : constraints)

declareConstraintSatisfaction' :: String -> Token -> Scope -> Scope
declareConstraintSatisfaction' className abstractName scope = declareInstance' className (contents abstractName) (Type (TypeHeader [] []) (TName abstractName)) scope

declareConstraintSatisfactions' :: [(String, Token)] -> Scope -> Scope
declareConstraintSatisfactions' [] scope = scope
declareConstraintSatisfactions' ((n, t) : cs) scope = declareConstraintSatisfactions' cs $ declareConstraintSatisfaction' n t scope

declareInstance' :: String -> String -> Type Parsed -> Scope -> Scope
declareInstance' className argumentName argumentType scope = scope { scopeInstances = ((className, argumentName), argumentType) : scopeInstances scope }

data Scoped x = Scoped (Scope -> (Scope, x))

instance Functor Scoped where
  fmap f (Scoped g) = Scoped $ \scope -> case g scope of (scope', y) -> (scope', f y)
instance Applicative Scoped where
  pure x = Scoped $ \scope -> (scope, x)
  Scoped f <*> Scoped g = Scoped $ \scope -> case f scope of (scope', f') -> case g scope' of (scope'', g') -> (scope'', f' g')
instance Monad Scoped where
  return = pure
  Scoped f >>= g = Scoped $ \scope -> case f scope of (scope', x) -> case g x of Scoped g' -> g' scope'

getScope :: Scoped Scope
getScope = Scoped $ \scope -> (scope, scope)

putScope :: Scope -> Scoped ()
putScope scope = Scoped $ \_ -> (scope, ())

modifyScope :: (Scope -> Scope) -> Scoped ()
modifyScope f = Scoped $ \scope -> (f scope, ())

fromScope :: (Scope -> a) -> Scoped a
fromScope f = Scoped $ \scope -> (scope, f scope)

subScoped :: Scoped x -> Scoped x
subScoped (Scoped f) = Scoped $ \scope -> let (_, y) = f scope in (scope, y)

-- This is the main thing.
runScoped :: Scope -> Scoped a -> (Scope, a)
runScoped scope (Scoped f) = f scope

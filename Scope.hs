
module Scope where
import AST
import Lexer

-- Scopes carry the information used to verify the program.

data ScopeVariableTypes = ScopeVariableTypes [(String, Type)] deriving Show

data ScopeClassDefinition = ScopeClassDefinition
  String           {-argument name-}
  Kind             {-argument term kind-}
  [String]         {-constraints on argument-}
  [(String, Type)] {-body items-}
  deriving Show

data ScopeTypeDefinition
  = ScopeTypeDefinitionAbstract Kind
  -- TODO: struct, enum
  deriving Show

data Scope = Scope
  { scopeVariableType :: [(String, Type)]
  , scopeClassDefinitions :: [(String, ScopeClassDefinition)]
  , scopeTypeDefinitions :: [(String, ScopeTypeDefinition)]
  , scopeFrozenStack :: [String]
  , scopeInstances :: [((String, String), Type)] -- ((class, typename), full argument spec)
  } deriving Show

variableTypeOf :: Scope -> String -> Maybe Type
variableTypeOf scope name = lookup name $ scopeVariableType scope

classDefinitionOf :: Scope -> String -> Maybe ScopeClassDefinition
classDefinitionOf scope name = lookup name $ scopeClassDefinitions scope

typeDefinitionOf :: Scope -> String -> Maybe ScopeTypeDefinition
typeDefinitionOf scope name = lookup name $ scopeTypeDefinitions scope

isVariableFrozen :: Scope -> String -> Bool
isVariableFrozen scope name = name `elem` scopeFrozenStack scope

hasClassInstance :: Scope -> String -> String -> Bool
hasClassInstance scope className argumentName = (className, argumentName) `elem` map fst (scopeInstances scope)

isIdentifierDefined :: Scope -> String -> Bool
isIdentifierDefined scope name
  | name `elem` (map fst $ scopeVariableType scope) = True
  | name `elem` (map fst $ scopeClassDefinitions scope) = True
  | name `elem` (map fst $ scopeTypeDefinitions scope) = True
  | otherwise = False


nameOfType :: Type -> Maybe Token
nameOfType (Type (TypeHeader free _) term) = search term where
  search (TName name)
    |contents name `elem` map (contents . fst) free = Just name
    |otherwise = Nothing
  search (left `TApply` _) = search left

declareVariable :: Scope -> String -> Type -> Scope
declareVariable scope n t = scope { scopeVariableType = (n, t) : scopeVariableType scope }

declareVariables :: Scope -> [(String, Type)] -> Scope
declareVariables scope [] = scope
declareVariables scope ((n, t) : rest) = declareVariables (declareVariable scope n t) rest

declareFrozenVariable :: Scope -> String -> Type -> Scope
declareFrozenVariable scope name typ = freezeVariable (declareFrozenVariable scope name typ) name

declareAbstractType :: Scope -> (String, Kind) -> Scope
declareAbstractType scope (name, kind) = scope{scopeTypeDefinitions = (name, ScopeTypeDefinitionAbstract kind) : scopeTypeDefinitions scope}

declareAbstractTypes :: Scope -> [(String, Kind)] -> Scope
declareAbstractTypes scope [] = scope
declareAbstractTypes scope (x : xs) = declareAbstractTypes (declareAbstractType scope x) xs

declareClass :: Scope -> Token -> Token -> Kind -> [Token] -> [(Token, Type)] -> Scope
declareClass scope name argument kind supers members = declareVariables (scope
  { scopeClassDefinitions = (contents name, ScopeClassDefinition (contents argument) kind (map contents supers) members') : scopeClassDefinitions scope
  }) members'
  where
  members' = map updateMember members
  updateMember (n, t) = (contents n, updateType t)
  updateType (Type header term) = Type (updateHeader header) term
  updateHeader (TypeHeader free constraints) = TypeHeader ((argument, kind) : free) (ClassConstraint argument name : constraints)

declareInstance :: Scope -> String -> String -> Type -> Scope
declareInstance scope className argumentName t = scope {
  scopeInstances = ((className, argumentName), t) : scopeInstances scope
  }

freezeVariable :: Scope -> String -> Scope
freezeVariable scope var = scope {
  scopeFrozenStack = var : scopeFrozenStack scope
  }

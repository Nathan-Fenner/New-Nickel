
module AST where
import Lexer

data Kind
  = Concrete
  | Higher Kind Kind
  deriving Show

data Term
  = TName Token
  | TApply Term Term
  deriving Show

-- TODO: multiparameter typeclasses
data Constraint = ClassConstraint Token{-free-} Token{-class-} deriving Show

data TypeHeader = TypeHeader [(Token, Kind)] [Constraint] deriving Show

data Type = Type TypeHeader Term deriving Show

data LetDeclaration
  = LetVarDeclare Token Type Expression
  deriving Show

data Reference
  = RName Token
  | RDot Reference Token
  | RAccess Reference Expression
  deriving Show
data Statement
  = VarDeclare Token Type (Maybe Expression)
  | If Expression [Statement] [Statement]
  | While Expression [Statement] -- TODO: "else"
  -- TODO: for
  | Let [LetDeclaration]
  | VarAssign Reference Expression
  | Do Expression
  deriving Show

data ClassStatement
  = ClassDeclare Token Type -- TODO: defaults
  deriving Show

data InstanceStatement
  = ClassImplement Token Expression
  deriving Show

data ModuleDeclaration
  = ModuleLet LetDeclaration
  | ClassDefinition TypeHeader Token Token [ClassStatement]
  | ClassInstance Type Token [InstanceStatement]
  deriving Show

data Expression
  = LiteralNumber Double
  | LiteralString String
  | Application Expression Expression
  | Function TypeHeader [(Token, Term)] Term [Statement]
  | Reference Reference
  deriving Show

data Module = Module Token [ModuleDeclaration] deriving Show


module AST where
import Lexer

data Kind
  = Concrete
  | Higher Kind Kind
  deriving Show

data Term status
  = TName Token
  | TApply (Term status) (Term status)
  deriving Show

-- TODO: multiparameter typeclasses
data Constraint status = ClassConstraint Token{-free-} Token{-class-} deriving Show

data TypeHeader status = TypeHeader [(Token, Kind)] [Constraint status] deriving Show

data Type status = Type (TypeHeader status) (Term status) deriving Show

data LetDeclaration status
  = LetVarDeclare Token (Type status) (Expression status)
  deriving Show

data Reference status
  = RName Token
  | RDot (Reference status) Token
  | RAccess (Reference status) (Expression status)
  deriving Show
data Statement status
  = VarDeclare Token (Type status) (Maybe (Expression status))
  | If (Expression status) [Statement status] [Statement status]
  | While (Expression status) [Statement status] -- TODO: "else"
  -- TODO: for
  | Let [LetDeclaration status]
  | VarAssign (Reference status) (Expression status)
  | Do (Expression status)
  deriving Show

data ClassStatement status
  = ClassDeclare Token (Type status) -- TODO: defaults
  deriving Show

data InstanceStatement status
  = ClassImplement Token (Expression status)
  deriving Show

data ModuleDeclaration status
  = ModuleLet (LetDeclaration status)
  | ClassDefinition (TypeHeader status) Token Token [ClassStatement status]
  | ClassInstance (Type status) Token [InstanceStatement status]
  deriving Show

data Expression status
  = LiteralNumber Double -- 1..0
  | LiteralString String -- "1"
  | Application (Expression status) (Expression status) -- print 5
  | Function (TypeHeader status) [(Token, Term status)] (Term status) [Statement status]
  | Reference (Reference status) -- x.y[3].z
  | Access (Expression status) (Expression status) -- (map (+1) list)[5]
  | Dot (Expression status) Token -- (blah + 1).blah
  deriving Show

data Module status = Module Token [ModuleDeclaration status] deriving Show

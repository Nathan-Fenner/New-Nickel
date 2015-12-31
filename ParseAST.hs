
module ParseAST where
import Lexer
import AST
import Parse

data Parsed

parseParens :: Parse x -> Parse x -> Parse x
parseParens inside outside = check LexSpecial "(" ??? (do value <- inside; _ <- expect LexSpecial ")"; return value, outside)

parseBetween :: Parse x -> Parse (Maybe loc) -> (x -> loc -> x -> x) -> Parse x
parseBetween atom op comb = self where
  self = do
    left <- atom
    op &&& (\here -> do right <- self; return $ comb left here right, return left)

parseCommas :: Parse x -> Parse [x]
parseCommas atom = parseBetween (fmap (:[]) atom) (checkAt LexSpecial ",") (\x _ xs -> x ++ xs)

parseBraces :: Parse x -> Parse [x]
parseBraces item = do
  _ <- expect LexSpecial "{"
  interior
  where
  interior = checkAt LexSpecial "}" &&& (\_ -> return [], do
    x <- item
    xs <- interior
    return $ x : xs
    )

parseKindConcrete :: Parse Kind
parseKindConcrete = expect LexSpecial "type" >> return Concrete
parseKindAtom :: Parse Kind
parseKindAtom = parseParens parseKind parseKindConcrete
parseKindArrow :: Parse Kind
parseKindArrow = parseBetween parseKindAtom (checkAt LexOperator "->") (\left _ right -> Higher left right)
parseKind :: Parse Kind
parseKind = parseKindArrow

parseTermName :: Parse (Term Parsed)
parseTermName = parseClass LexName >>= return . TName
parseTermAtom :: Parse (Term Parsed)
parseTermAtom = parseParens parseTerm parseTermName
parseTermArrow :: Parse (Term Parsed)
parseTermArrow = parseBetween parseTermAtom (checkAt LexOperator "->") (\left here right -> (TName (Token "->" here) `TApply` left) `TApply` right)
parseTerm :: Parse (Term Parsed)
parseTerm = parseTermArrow

parseConstraint :: Parse (Constraint Parsed)
parseConstraint = do
  free <- parseClass LexName
  _ <- expect LexSpecial "~"
  classname <- parseClass LexName
  return (ClassConstraint free classname)

parseTypeHeaderFreeOne :: Parse (Token, Kind)
parseTypeHeaderFreeOne = do
  free <- parseClass LexName
  checkAt LexSpecial ":" &&& (\_ -> do kind <- parseKind; return (free, kind), return (free, Concrete))
parseTypeHeaderFree :: Parse [(Token, Kind)]
parseTypeHeaderFree = parseCommas parseTypeHeaderFreeOne
parseTypeHeaderConstraint :: Parse [Constraint Parsed]
parseTypeHeaderConstraint = parseCommas parseConstraint
parseTypeHeaderBody :: Parse (TypeHeader Parsed)
parseTypeHeaderBody = do
  free <- parseTypeHeaderFree
  checkAt LexSpecial "|" &&& (\_ -> do constraint <- parseTypeHeaderConstraint; return $ TypeHeader free constraint, return $ TypeHeader free [])
parseTypeHeader :: Parse (TypeHeader Parsed)
parseTypeHeader = do
  checkAt LexSpecial "[" &&& (\_ -> do answer <- parseTypeHeaderBody; _ <- expect LexSpecial "]"; return answer, return $ TypeHeader [] [])

parseType :: Parse (Type Parsed)
parseType = do
  header <- parseTypeHeader
  term <- parseTerm
  return $ Type header term

parseLetVarDeclare :: Parse (Maybe (LetDeclaration Parsed))
parseLetVarDeclare = checkAt LexSpecial "var" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  _ <- expect LexOperator "="
  e <- parseExpression
  _ <- expect LexSpecial ";"
  return $ LetVarDeclare name t e
parseLetDeclaration :: Parse (Maybe (LetDeclaration Parsed))
parseLetDeclaration = parseLetVarDeclare -- TODO: "func"
parseLetBody :: Parse [LetDeclaration Parsed]
parseLetBody = do
  _ <- expect LexSpecial "{"
  body <- parseMany parseLetDeclaration
  _ <- expect LexSpecial "}"
  return body

parseReferenceName :: Parse (Maybe (Reference Parsed))
parseReferenceName = do
  name <- checkClass LexName
  return $ name >>= Just . RName
parseReferenceSuffixDot :: Parse (Maybe Token)
parseReferenceSuffixDot = checkAt LexSpecial "." ### do
  name <- parseClass LexName
  return name
parseReferenceSuffixAccess :: Parse (Maybe (Expression Parsed))
parseReferenceSuffixAccess = checkAt LexSpecial "[" ### do
  e <- parseExpression
  _ <- expect LexSpecial "]"
  return e
parseReference :: Parse (Maybe (Reference Parsed))
parseReference = parseReferenceName ##> \base -> do
  addSuffixes base
  where
  addSuffixes base = do
    refDot <- parseReferenceSuffixDot
    case refDot of
      Just name -> addSuffixes (base `RDot` name)
      Nothing -> do
        refAccess <- parseReferenceSuffixAccess
        case refAccess of
          Just e -> addSuffixes (base `RAccess` e)
          Nothing -> return base

parseStatementVarDeclareBody :: Parse (Maybe (Statement Parsed))
parseStatementVarDeclareBody = checkAt LexSpecial "var" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  e <- checkAt LexOperator "=" ### parseExpression
  _ <- expect LexSpecial ";"
  return $ VarDeclare name t e
parseStatementIf :: Parse (Maybe (Statement Parsed))
parseStatementIf = checkAt LexSpecial "if" ### do
  cond <- parseExpression
  body <- parseBlock
  elseClause <- (checkAt LexSpecial "else" ### parseBlock) ||> return [] -- TODO: else-if
  return $ If cond body elseClause
parseStatementWhile :: Parse (Maybe (Statement Parsed))
parseStatementWhile = checkAt LexSpecial "while" ### do
  cond <- parseExpression
  body <- parseBlock
  return $ While cond body -- TODO: parse else
parseStatementLet :: Parse (Maybe (Statement Parsed))
parseStatementLet = checkAt LexSpecial "let" ### do
  body <- parseLetBody
  return $ Let body
parseStatementUnmarked :: Parse (Statement Parsed)
parseStatementUnmarked = do
  e <- parseExpressionAssign -- allows assignment, but only at the root.
  r <- transform e
  _ <- expect LexSpecial ";"
  return r
  where
  transform (EOpLeaf e) = return $ Do e
  transform (EOpBranch (EOpLeaf (Reference reference)) (Token "=" _) (EOpLeaf expression)) = return $ VarAssign reference expression
  transform (EOpBranch (EOpLeaf left) (Token "=" _) _) = parseError $ Error $ "The left side of an `=` must be a reference, not " ++ show left -- TODO: print nicer
  transform _ = parseError $ Error $ "There can be only one assignment for each line."
  -- transform = undefined -- TODO: transform (really important!)
  -- We'll have to decide how to parse assignments
parseStatement :: Parse (Statement Parsed)
parseStatement = parseStatementVarDeclareBody ||| parseStatementIf ||| parseStatementWhile ||| parseStatementLet ||> parseStatementUnmarked

parseBlock :: Parse [Statement Parsed]
parseBlock = parseBraces parseStatement

parseClassStatementDeclare :: Parse (ClassStatement Parsed)
parseClassStatementDeclare = do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  _ <- expect LexSpecial ";"
  return $ ClassDeclare name t
parseClassStatement :: Parse (ClassStatement Parsed)
parseClassStatement = parseClassStatementDeclare
parseClassBody :: Parse [ClassStatement Parsed]
parseClassBody = parseBraces parseClassStatement

parseInstanceStatementImplement :: Parse (InstanceStatement Parsed)
parseInstanceStatementImplement = do -- TODO: add 'func' definitions
  name <- parseClass LexName
  _ <- expect LexOperator "="
  e <- parseExpression
  _ <- expect LexSpecial ";"
  return $ ClassImplement name e
parseInstanceStatement :: Parse (InstanceStatement Parsed)
parseInstanceStatement = parseInstanceStatementImplement

parseInstanceBody :: Parse [InstanceStatement Parsed]
parseInstanceBody = parseBraces parseInstanceStatement

parseModuleLet :: Parse (Maybe (ModuleDeclaration Parsed))
parseModuleLet = fmap (fmap ModuleLet) parseLetDeclaration
parseModuleClassDefinition :: Parse (Maybe (ModuleDeclaration Parsed))
parseModuleClassDefinition = checkAt LexSpecial "class" ### do
  header <- parseTypeHeader
  name <- parseClass LexName -- TODO: multiparameter type classes
  _ <- expect LexSpecial "~"
  className <- parseClass LexName
  body <- parseClassBody
  return $ ClassDefinition header name className body
parseModuleInstanceDefinition :: Parse (Maybe (ModuleDeclaration Parsed))
parseModuleInstanceDefinition = checkAt LexSpecial "instance" ### do
  t <- parseType
  _ <- expect LexSpecial "~"
  className <- parseClass LexName
  body <- parseInstanceBody
  return $ ClassInstance t className body
parseModuleDeclaration :: Parse (Maybe (ModuleDeclaration Parsed))
parseModuleDeclaration = parseModuleClassDefinition ||| parseModuleInstanceDefinition ||| parseModuleLet


parseExpressionLiteralNumber :: Parse (Maybe (Expression Parsed))
parseExpressionLiteralNumber = do
  number <- checkClass LexNumber
  return (number >>= Just . LiteralNumber . read . contents) -- TODO: something safer than "read"
parseExpressionLiteralString :: Parse (Maybe (Expression Parsed))
parseExpressionLiteralString = do
  string <- checkClass LexString
  return (string >>= Just . LiteralString . contents)
parseExpressionLiteral :: Parse (Maybe (Expression Parsed))
parseExpressionLiteral = parseExpressionLiteralNumber ||| parseExpressionLiteralString
parseExpressionFunction :: Parse (Maybe (Expression Parsed))
parseExpressionFunctionArgument :: Parse (Maybe (Token, Term Parsed))
parseExpressionFunctionArgument = checkAt LexSpecial "(" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  term <- parseTerm
  _ <- expect LexSpecial ")"
  return (name, term)
parseExpressionFunctionArguments :: Parse [(Token, Term Parsed)]
parseExpressionFunctionArguments = parseMany parseExpressionFunctionArgument
parseExpressionFunctionReturn :: Parse (Term Parsed)
parseExpressionFunctionReturn = (checkAt LexOperator "->" ### parseTerm) ||> (do here <- parseHere; return $ TName $ Token "Unit" here)
parseExpressionFunction = checkAt LexSpecial "func" ### do
  -- we expect a type header to introduce variables, then the arguments, then a return value
  header <- parseTypeHeader
  arguments <- parseExpressionFunctionArguments
  returnType <- parseExpressionFunctionReturn
  block <- parseBlock
  return $ Function header arguments returnType block
parseExpressionReference :: Parse (Maybe (Expression Parsed))
parseExpressionReference = parseReference ##> \reference -> do
  return $ Reference reference
parseExpressionAtom :: Parse (Maybe (Expression Parsed))
parseExpressionAtom = parseParens (fmap Just parseExpression) $ parseExpressionLiteral ||| parseExpressionFunction ||| parseExpressionReference
parseExpressionAtomAccess :: Parse (Maybe (Expression Parsed))
parseExpressionAtomAccess = parseExpressionAtom ##> \atom -> parseSuffixes atom
  where
  parseSuffixDot :: Parse (Maybe Token)
  parseSuffixDot = checkAt LexSpecial "." ### parseClass LexName
  parseSuffixAccess :: Parse (Maybe (Expression Parsed))
  parseSuffixAccess = checkAt LexSpecial "[" ##> \openBrace -> do
    index <- parseExpression
    close <- checkAt LexSpecial "]"
    case close of
      Just _ -> return index
      Nothing -> parseError $ Error $ "expected a `]` to close `[` opened at " ++ show openBrace ++ "."
  parseSuffix :: Parse (Maybe (Either Token (Expression Parsed)))
  parseSuffix = fmap (fmap Left) parseSuffixDot ||| fmap (fmap Right) parseSuffixAccess
  parseSuffixes :: Expression Parsed -> Parse (Expression Parsed)
  parseSuffixes e = do
    suffix <- parseSuffix
    case suffix of
      Nothing -> return e
      Just (Left x) -> parseSuffixes (Dot e x)
      Just (Right x) -> parseSuffixes (Access e x)

parseExpressionApplication :: Parse (Maybe (Expression Parsed))
parseExpressionApplication = do
  collection <- parseMany parseExpressionAtom
  case collection of
    [] -> return Nothing
    (e : es) -> return $ combine e es
  where
  combine e [] = Just e
  combine e (x:xs) = combine (e `Application` x) xs
data OpDirection = ARight | ALeft deriving Show
data ExpressionOp = EOpLeaf (Expression Parsed) | EOpBranch ExpressionOp Token ExpressionOp deriving Show
rotateLeft :: ExpressionOp -> ExpressionOp
rotateLeft (EOpBranch (EOpBranch left op middle) op' right) = rotateLeft $ EOpBranch left op (EOpBranch middle op' right)
rotateLeft tree = tree
parseExpressionOperatorTree :: Parse (Expression Parsed) -> [String] -> [(Maybe OpDirection, [String])] -> Parse ExpressionOp
parseExpressionOperatorTree follow ops rest = do
  left <- next
  checkAny LexOperator ops &&& (\op -> do
    right <- this
    return $ EOpBranch (EOpLeaf left) op right, return $ EOpLeaf left)
  where
  this = parseExpressionOperatorTree follow ops rest
  next = parseExpressionOperator follow rest
parseExpressionOperatorPrefix :: Parse (Expression Parsed) -> [String] -> [(Maybe OpDirection, [String])] -> Parse (Expression Parsed)
parseExpressionOperatorPrefix follow pre rest = checkAny LexOperator pre &&& (\op -> do
  right <- this
  return $ Reference (RName op) `Application` right
  , next)
  where
  this = parseExpressionOperatorPrefix follow pre rest
  next = parseExpressionOperator follow rest
parseExpressionOperator :: Parse (Expression Parsed) -> [(Maybe OpDirection, [String])] -> Parse (Expression Parsed)
parseExpressionOperator follow [] = follow
parseExpressionOperator follow ((Nothing, ops) : rest) = parseExpressionOperatorPrefix follow ops rest
parseExpressionOperator follow ((Just dir, ops) : rest) = do
  tree <- parseExpressionOperatorTree follow ops rest
  return $ flatten $ case dir of
    ALeft -> rotateLeft tree
    ARight -> tree
  where
  flatten :: ExpressionOp -> Expression Parsed
  flatten (EOpLeaf e) = e
  flatten (EOpBranch left op right) = (Reference (RName op) `Application` flatten left) `Application` flatten right
parseExpression :: Parse (Expression Parsed)
parseExpression = parseExpressionOperator (parseExpressionApplication ||> parseError (Error $ "an expression is expected"))
  [ (Just ARight, ["|>"])
  , (Just ALeft, ["<>"])
  , (Just ALeft, ["||"])
  , (Just ALeft, ["&&"])
  , (Just ALeft, ["==", "~=", "<", ">", "<=", ">="])
  , (Just ARight, ["++"])
  , (Just ARight, ["+", "-"])
  , (Just ARight, ["*", "/"])
  , (Nothing, ["+", "-"])
  , (Just ARight, ["^", "**"])
  ]
parseExpressionAssign :: Parse ExpressionOp
parseExpressionAssign = parseExpressionOperatorTree parseExpression ["="] []

parseModule :: Parse (Module Parsed)
parseModule = do
  _ <- expect LexSpecial "module"
  name <- parseClass LexName
  _ <- expect LexSpecial "where"
  declarations <- parseMany parseModuleDeclaration
  return $ Module name declarations

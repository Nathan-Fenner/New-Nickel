
module ParseAST where
import Lexer
import AST
import Parse

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

parseTermName :: Parse Term
parseTermName = parseClass LexName >>= return . TName
parseTermAtom :: Parse Term
parseTermAtom = parseParens parseTerm parseTermName
parseTermArrow :: Parse Term
parseTermArrow = parseBetween parseTermAtom (checkAt LexOperator "->") (\left here right -> (TName (Token "->" here) `TApply` left) `TApply` right)
parseTerm :: Parse Term
parseTerm = parseTermArrow

parseConstraint :: Parse Constraint
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
parseTypeHeaderConstraint :: Parse [Constraint]
parseTypeHeaderConstraint = parseCommas parseConstraint
parseTypeHeaderBody :: Parse TypeHeader
parseTypeHeaderBody = do
  free <- parseTypeHeaderFree
  checkAt LexSpecial "|" &&& (\_ -> do constraint <- parseTypeHeaderConstraint; return $ TypeHeader free constraint, return $ TypeHeader free [])
parseTypeHeader :: Parse TypeHeader
parseTypeHeader = do
  checkAt LexSpecial "[" &&& (\_ -> do answer <- parseTypeHeaderBody; _ <- expect LexSpecial "]"; return answer, return $ TypeHeader [] [])

parseType :: Parse Type
parseType = do
  header <- parseTypeHeader
  term <- parseTerm
  return $ Type header term

parseLetVarDeclare :: Parse (Maybe LetDeclaration)
parseLetVarDeclare = checkAt LexSpecial "var" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  _ <- expect LexOperator "="
  e <- parseExpression
  _ <- expect LexSpecial ";"
  return $ LetVarDeclare name t e
parseLetDeclaration :: Parse (Maybe LetDeclaration)
parseLetDeclaration = parseLetVarDeclare -- TODO: "func"
parseLetBody :: Parse [LetDeclaration]
parseLetBody = do
  _ <- expect LexSpecial "{"
  body <- parseMany parseLetDeclaration
  _ <- expect LexSpecial "}"
  return body

parseReferenceName :: Parse (Maybe Reference)
parseReferenceName = do
  name <- checkClass LexName
  return $ name >>= Just . RName
parseReferenceSuffixDot :: Parse (Maybe Token)
parseReferenceSuffixDot = checkAt LexSpecial "." ### do
  name <- parseClass LexName
  return name
parseReferenceSuffixAccess :: Parse (Maybe Expression)
parseReferenceSuffixAccess = checkAt LexSpecial "[" ### do
  e <- parseExpression
  _ <- expect LexSpecial "]"
  return e
parseReference :: Parse (Maybe Reference)
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

parseStatementVarDeclareBody :: Parse (Maybe Statement)
parseStatementVarDeclareBody = checkAt LexSpecial "var" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  e <- checkAt LexOperator "=" ### parseExpression
  _ <- expect LexSpecial ";"
  return $ VarDeclare name t e
parseStatementIf :: Parse (Maybe Statement)
parseStatementIf = checkAt LexSpecial "if" ### do
  cond <- parseExpression
  body <- parseBlock
  elseClause <- (checkAt LexSpecial "else" ### parseBlock) ||> return [] -- TODO: else-if
  return $ If cond body elseClause
parseStatementWhile :: Parse (Maybe Statement)
parseStatementWhile = checkAt LexSpecial "while" ### do
  cond <- parseExpression
  body <- parseBlock
  return $ While cond body -- TODO: parse else
parseStatementLet :: Parse (Maybe Statement)
parseStatementLet = checkAt LexSpecial "let" ### do
  body <- parseLetBody
  return $ Let body
parseStatementUnmarked :: Parse Statement
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
parseStatement :: Parse Statement
parseStatement = parseStatementVarDeclareBody ||| parseStatementIf ||| parseStatementWhile ||| parseStatementLet ||> parseStatementUnmarked

parseBlock :: Parse [Statement]
parseBlock = parseBraces parseStatement

parseClassStatementDeclare :: Parse ClassStatement
parseClassStatementDeclare = do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  t <- parseType
  _ <- expect LexSpecial ";"
  return $ ClassDeclare name t
parseClassStatement :: Parse ClassStatement
parseClassStatement = parseClassStatementDeclare
parseClassBody :: Parse [ClassStatement]
parseClassBody = parseBraces parseClassStatement

parseInstanceStatementImplement :: Parse InstanceStatement
parseInstanceStatementImplement = do -- TODO: add 'func' definitions
  name <- parseClass LexName
  _ <- expect LexOperator "="
  e <- parseExpression
  _ <- expect LexSpecial ";"
  return $ ClassImplement name e
parseInstanceStatement :: Parse InstanceStatement
parseInstanceStatement = parseInstanceStatementImplement

parseInstanceBody :: Parse [InstanceStatement]
parseInstanceBody = parseBraces parseInstanceStatement

parseModuleLet :: Parse (Maybe ModuleDeclaration)
parseModuleLet = fmap (fmap ModuleLet) parseLetDeclaration
parseModuleClassDefinition :: Parse (Maybe ModuleDeclaration)
parseModuleClassDefinition = checkAt LexSpecial "class" ### do
  header <- parseTypeHeader
  name <- parseClass LexName -- TODO: multiparameter type classes
  _ <- expect LexSpecial "~"
  className <- parseClass LexName
  body <- parseClassBody
  return $ ClassDefinition header name className body
parseModuleInstanceDefinition :: Parse (Maybe ModuleDeclaration)
parseModuleInstanceDefinition = checkAt LexSpecial "instance" ### do
  t <- parseType
  _ <- expect LexSpecial "~"
  className <- parseClass LexName
  body <- parseInstanceBody
  return $ ClassInstance t className body
parseModuleDeclaration :: Parse (Maybe ModuleDeclaration)
parseModuleDeclaration = parseModuleClassDefinition ||| parseModuleInstanceDefinition ||| parseModuleLet


parseExpressionLiteralNumber :: Parse (Maybe Expression)
parseExpressionLiteralNumber = do
  number <- checkClass LexNumber
  return (number >>= Just . LiteralNumber . read . contents) -- TODO: something safer than "read"
parseExpressionLiteralString :: Parse (Maybe Expression)
parseExpressionLiteralString = do
  string <- checkClass LexString
  return (string >>= Just . LiteralString . contents)
parseExpressionLiteral :: Parse (Maybe Expression)
parseExpressionLiteral = parseExpressionLiteralNumber ||| parseExpressionLiteralString
parseExpressionFunction :: Parse (Maybe Expression)
parseExpressionFunctionArgument :: Parse (Maybe (Token, Term))
parseExpressionFunctionArgument = checkAt LexSpecial "(" ### do
  name <- parseClass LexName
  _ <- expect LexSpecial ":"
  term <- parseTerm
  _ <- expect LexSpecial ")"
  return (name, term)
parseExpressionFunctionArguments :: Parse [(Token, Term)]
parseExpressionFunctionArguments = parseMany parseExpressionFunctionArgument
parseExpressionFunctionReturn :: Parse Term
parseExpressionFunctionReturn = (checkAt LexOperator "->" ### parseTerm) ||> (do here <- parseHere; return $ TName $ Token "Unit" here)
parseExpressionFunction = checkAt LexSpecial "func" ### do
  -- we expect a type header to introduce variables, then the arguments, then a return value
  header <- parseTypeHeader
  arguments <- parseExpressionFunctionArguments
  returnType <- parseExpressionFunctionReturn
  block <- parseBlock
  return $ Function header arguments returnType block
parseExpressionReference :: Parse (Maybe Expression)
parseExpressionReference = parseReference ##> \reference -> do
  return $ Reference reference
parseExpressionAtom :: Parse (Maybe Expression)
parseExpressionAtom = parseParens (fmap Just parseExpression) $ parseExpressionLiteral ||| parseExpressionFunction ||| parseExpressionReference
parseExpressionAtomAccess :: Parse (Maybe Expression)
parseExpressionAtomAccess = parseExpressionAtom ##> \atom -> parseSuffixes atom
  where
  parseSuffixDot :: Parse (Maybe Token)
  parseSuffixDot = checkAt LexSpecial "." ### parseClass LexName
  parseSuffixAccess :: Parse (Maybe Expression)
  parseSuffixAccess = checkAt LexSpecial "[" ##> \openBrace -> do
    index <- parseExpression
    close <- checkAt LexSpecial "]"
    case close of
      Just _ -> return index
      Nothing -> parseError $ Error $ "expected a `]` to close `[` opened at " ++ show openBrace ++ "."
  parseSuffix :: Parse (Maybe (Either Token Expression))
  parseSuffix = fmap (fmap Left) parseSuffixDot ||| fmap (fmap Right) parseSuffixAccess
  parseSuffixes :: Expression -> Parse Expression
  parseSuffixes e = do
    suffix <- parseSuffix
    case suffix of
      Nothing -> return e
      Just (Left x) -> parseSuffixes (Dot e x)
      Just (Right x) -> parseSuffixes (Access e x)

parseExpressionApplication :: Parse (Maybe Expression)
parseExpressionApplication = do
  collection <- parseMany parseExpressionAtom
  case collection of
    [] -> return Nothing
    (e : es) -> return $ combine e es
  where
  combine e [] = Just e
  combine e (x:xs) = combine (e `Application` x) xs
data OpDirection = ARight | ALeft deriving Show
data ExpressionOp = EOpLeaf Expression | EOpBranch ExpressionOp Token ExpressionOp deriving Show
rotateLeft :: ExpressionOp -> ExpressionOp
rotateLeft (EOpBranch (EOpBranch left op middle) op' right) = rotateLeft $ EOpBranch left op (EOpBranch middle op' right)
rotateLeft tree = tree
parseExpressionOperatorTree :: Parse Expression -> [String] -> [(Maybe OpDirection, [String])] -> Parse ExpressionOp
parseExpressionOperatorTree follow ops rest = do
  left <- next
  checkAny LexOperator ops &&& (\op -> do
    right <- this
    return $ EOpBranch (EOpLeaf left) op right, return $ EOpLeaf left)
  where
  this = parseExpressionOperatorTree follow ops rest
  next = parseExpressionOperator follow rest
parseExpressionOperatorPrefix :: Parse Expression -> [String] -> [(Maybe OpDirection, [String])] -> Parse Expression
parseExpressionOperatorPrefix follow pre rest = checkAny LexOperator pre &&& (\op -> do
  right <- this
  return $ Reference (RName op) `Application` right
  , next)
  where
  this = parseExpressionOperatorPrefix follow pre rest
  next = parseExpressionOperator follow rest
parseExpressionOperator :: Parse Expression -> [(Maybe OpDirection, [String])] -> Parse Expression
parseExpressionOperator follow [] = follow
parseExpressionOperator follow ((Nothing, ops) : rest) = parseExpressionOperatorPrefix follow ops rest
parseExpressionOperator follow ((Just dir, ops) : rest) = do
  tree <- parseExpressionOperatorTree follow ops rest
  return $ flatten $ case dir of
    ALeft -> rotateLeft tree
    ARight -> tree
  where
  flatten :: ExpressionOp -> Expression
  flatten (EOpLeaf e) = e
  flatten (EOpBranch left op right) = (Reference (RName op) `Application` flatten left) `Application` flatten right
parseExpression :: Parse Expression
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

parseModule :: Parse Module
parseModule = do
  _ <- expect LexSpecial "module"
  name <- parseClass LexName
  _ <- expect LexSpecial "where"
  declarations <- parseMany parseModuleDeclaration
  return $ Module name declarations




module Parse where
import Control.Applicative

import Lexer


data Parse t = Parse {run :: ([(LexerClass, Token)] -> Either Error (t, [(LexerClass, Token)]))}

data Error = Error String deriving Show

errorOf :: Error -> String
errorOf (Error x) = x

instance Functor Parse where
  fmap f (Parse fun) = Parse $ \stream -> case fun stream of
    Left message -> Left message
    Right (result, stream') -> Right (f result, stream')

instance Applicative Parse where
  pure x = Parse $ \stream -> Right (x, stream)
  Parse f <*> Parse x = Parse $ \stream -> case f stream of
    Left message -> Left message
    Right (f', stream') -> case x stream' of
      Left message -> Left message
      Right (x', stream'') -> Right (f' x', stream'')

instance Monad Parse where
  return = pure
  Parse x >>= f = Parse $ \stream -> case x stream of
    Left message -> Left message
    Right (x', stream') -> run (f x') stream'

parseError :: Error -> Parse x
parseError message = Parse $ \stream -> Left (Error (errorOf message ++ " at " ++ locOf stream))
  where
  locOf [] = "???"
  locOf tokens@((_, Token _ here) : _) = here ++ "; " ++ unwords (take 10 (map (contents . snd) tokens))

parseClass :: LexerClass -> Parse Token
parseClass c = Parse go where
  go [] = Left (Error $ "expected a " ++ show c ++ " but reached end of stream")
  go ((c', t) : rest)
    |c == c' = Right (t, rest)
    |otherwise = Left (Error $ "expected a " ++ show c ++ " but got a " ++ show c' ++ " (" ++ show t ++ ")")

expect :: LexerClass -> String -> Parse String -- location
expect c name = Parse go where
  go [] = Left (Error $ "expected a " ++ show c ++ " but reached end of stream")
  go ((c', t) : rest)
    |c == c' && contents t == name = Right (location t, rest)
    |otherwise = Left (Error $ "expected `" ++ name ++ "` but got `" ++ contents t ++ "` (" ++ show t ++ ")")


check :: LexerClass -> String -> Parse Bool
check c name = Parse go where
  go [] = Right (False, [])
  go whole@((c', Token name' _) : rest)
    |c == c' && name == name' = Right (True, rest)
    |otherwise = Right (False, whole)

checkClass :: LexerClass -> Parse (Maybe Token)
checkClass c = Parse go where
  go [] = Right (Nothing, [])
  go whole@((c', t) : rest)
    |c == c' = Right (Just t, rest)
    |otherwise = Right (Nothing, whole)

checkAt :: LexerClass -> String -> Parse (Maybe Location)
checkAt c name = Parse go where
  go [] = Right (Nothing, [])
  go whole@((c', Token name' at) : rest)
    |c == c' && name == name' = Right (Just at, rest)
    |otherwise = Right (Nothing, whole)

checkAny :: LexerClass -> [String] -> Parse (Maybe Token)
checkAny _ [] = return Nothing
checkAny c (x:xs) = checkAt c x &&& (\l -> return $ Just $ Token x l, checkAny c xs)

parseHere :: Parse Location
parseHere = Parse go where
  go [] = Right ("*nowhere*", [])
  go whole@((_, Token h _) : _) = Right (h, whole)

parseMany :: Parse (Maybe x) -> Parse [x]
parseMany p = do
  x <- p
  case x of
    Nothing -> return []
    Just y -> do
      ys <- parseMany p
      return $ y:ys

(???) :: Parse Bool -> (Parse x, Parse x) -> Parse x
cond ??? (yes, no) = do
  b <- cond
  case b of
    True -> yes
    False -> no

(&&&) :: Parse (Maybe x) -> (x -> Parse y, Parse y) -> Parse y
cond &&& (yes, no) = do
  b <- cond
  case b of
    Just b' -> yes b'
    Nothing -> no

(###) :: Parse (Maybe x) -> Parse y -> Parse (Maybe y)
cond ### yes = cond &&& (\_ -> fmap Just yes, return Nothing)

(##>) :: Parse (Maybe x) -> (x -> Parse y) -> Parse (Maybe y)
cond ##> yes = cond &&& (\x -> fmap Just $ yes x, return Nothing)

(|||) :: Parse (Maybe x) -> Parse (Maybe x) -> Parse (Maybe x)
cond ||| no = cond &&& (return . Just, no)

(||>) :: Parse (Maybe x) -> Parse x -> Parse x
cond ||> no = cond &&& (return, no)

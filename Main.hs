module Main where
import AST
import Lexer
import Parse
import ParseAST
import Verify

import System.Environment

build :: FilePath -> IO ()
build file = do
	source <- readFile file
	print $ run parseModule $ lexer file source

main :: IO ()
main = do
	args <- getArgs
	case args of
		["version"] -> putStrLn "nickel 0.0.1 (November 2015)"
		["help"] -> putStrLn "TODO"
		["build", file] -> build file
		_ -> putStrLn "Usage: version / help / build [file]"




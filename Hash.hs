module Main (runScript) where

import System.Directory
import System.FilePath.Posix (takeExtension)
import Control.Applicative ((<$>))
import Control.Monad
import Data.Foldable (foldrM)

import Parse (parseProgram)
import Exec (runHashProgram, getPath, runTopLevel, emptyScriptState, ScriptState(..))
import Commands (commands)

sExt = ".hash"
rExt = sExt ++ "rc"

runScript :: FilePath -> IO ()
runScript p = runRc >>= runFile sExt p >> return ()

runInteractive :: IO ()
runInteractive = runRc >>= \s -> getCurrentDirectory >>= n s >>= run
  where run ss = do
  	      p <- parseProgram <$> (putStr "> " >> getLine)
  	      case p of
  	      	Left   e    -> putStrLn "Couldn't parse given line" >> run ss
  	      	Right (e:_) -> runTopLevel commands ss e >>= run

runRc :: IO ScriptState
runRc = do
	home <- getHomeDirectory
	fs <- filter f <$> (getDirectoryContents home)
	-- mapM_ (runFile rExt getHomeDirectory) fs
	foldrM (\p s -> runFile rExt (getPath s p) s) (ss home) fs
	  where f = (==) rExt . takeExtension
                  

checkPath :: String -> FilePath -> IO ()
checkPath ext p = do
	e <- doesFileExist p
	if (not e) then fail $ "Given file doesn't exist " ++ p
		       else if (takeExtension p == ext) then return ()
		                                        else fail "File isn't a *Hash* script"

{-
runFile :: String -> IO FilePath -> FilePath -> IO ScriptState
runFile e fDir path = do
	checkPath e path
	p <- parseProgram <$> readFile path
	case p of
		Left   e -> fail $ "Error occured while parsing " ++ path
		Right ts -> runProgram ts
	where runProgram ts = do
		    dir <- fDir
		    runHashProgram commands (ss dir) ts
-}


runFile :: String -> FilePath -> ScriptState -> IO ScriptState
runFile e path s = do
	checkPath e path
	p <- parseProgram <$> readFile path
	case p of
		Left   e -> fail $ "Error occured while parsing " ++ path
		Right ts -> runHashProgram commands s ts

ss dir = emptyScriptState { wd = dir }
n s d = return $ s { wd = d }


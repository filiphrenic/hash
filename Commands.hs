module Commands (commands) where

import Control.Monad
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as M
import Data.List (intersperse)
import System.IO (writeFile)
import System.Directory
import System.FilePath.Posix ((</>), takeFileName, makeRelative, splitPath)

import Exec

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: CommandTable
commands = M.fromList list

type Name = String

list :: [(Name, Command)]
list = [("mv",     cMove)
       ,("cp",     cCopy)
       ,("rm",     cRemove)
       ,("create", cCreate)
       ,("ls",     cLs)
       ,("cd",     cCd)
       ,("pwd",    cPwd)
       ,("#",      cComment)
       ,("echo",   cEcho)
       ,("assign", cAssign)
       ,("cat",    cCat)
       ,("mvdir",  cDirMove)
       ,("cpdir",  cDirCopy)
       ,("mkdir",  cDirMake)
       ,("rmdir",  cDirRemove)
       ]

-- Implementation of commands ==========================================

-- files
cMove :: Command
cMove = cAbstractFile moveFile 2

cCopy :: Command
cCopy = cAbstractFile copyFile 2

cRemove :: Command
cRemove = cAbstractFile (\s d -> removeFile s) 1

cCreate :: Command
cCreate = cAbstractFile (\s d -> createNewFile s) 1

-- directories
cDirMove :: Command
cDirMove = cAbstractDir moveFile removeDirectory

cDirCopy :: Command
cDirCopy = cAbstractDir copyFile (\x -> return ())

cDirMake :: Command
cDirMake = cDirCreateRemove createDirectoryR

cDirRemove :: Command
cDirRemove = cDirCreateRemove removeDirectoryRecursive

cPwd :: Command
cPwd _ ss = do
	print $ wd ss
	return ss

cCd :: Command
cCd []     ss = cPwd [] ss
cCd [path] ss = return ss { wd = getPath ss path }
cCd _      _  = fail "Must provide maximum 1 argument"

cLs :: Command
cLs []     ss = getHomeDirectory >>= listContent ss
cLs [path] ss = listContent ss $ getPath ss path
cls _      _  = fail "Must provide maximum 1 argument"

listContent :: ScriptState -> FilePath -> IO ScriptState
listContent ss path = do
	nOutput <- unlines <$> (reverse <$> getDirectoryContents path)
	return ss { output = nOutput }

cComment :: Command
cComment _ ss = return ss

cEcho :: Command
cEcho [] ss = fail "Must provide unleast 1 argument"
cEcho xs ss = return ss { output = concat $ intersperse " " xs }

cAssign :: Command
cAssign [var, val] ss = return ss { vartable = M.insert var val $ vartable ss }
cAssign _          _  = fail "Only two arguments needed: variable name and it's value"

cCat :: Command
cCat [path] ss = nScript <$> readFile (getPath ss path)
  where nScript o = ss { output = o }
cCat _      _  = fail "Only one argument needed"

-- File methods ========================================================

createNewFile :: FilePath -> IO ()
createNewFile path = writeFile path ""

moveFile :: FilePath ->  FilePath -> IO ()
moveFile src dst = do
	copyFile src dst
	removeFile src

cDirCreateRemove :: (FilePath -> IO ()) -> Command
cDirCreateRemove f args ss = do
	mapM f args
	return ss

createDirectoryR :: FilePath -> IO ()
createDirectoryR p = do
	x <- doesDirectoryExist p
	if x then return ()
	     else recursive $ splitPath p
  where recursive [] = fail "You need to provide a path"
        recursive [p] = createDirIfDoesntExist p
        recursive (x:y:xs) = do
        	createDirIfDoesntExist x
        	recursive $ (x </> y):xs  

createDirIfDoesntExist :: FilePath -> IO ()
createDirIfDoesntExist p = doesDirectoryExist p >>= 
            (\b -> when (not b) $ createDirectory p) 

-- ====================================

cAbstractFile :: (FilePath -> FilePath -> IO()) -> Int -> Command
cAbstractFile f nArgs args ss = apply $ map (getPath ss) args
  where apply []         = err
        apply [_]        = err
        apply [dst,src]  = do
        	f src dst
        	return ss
        apply (dir:srcs) = do
        	mapM (\src -> f src $ dir </> takeFileName src) srcs
        	return ss
        err = fail ("Wrong input arguments, must provide unleast " ++ show nArgs)

cAbstractDir :: (FilePath -> FilePath -> IO ()) -> (FilePath -> IO ()) -> Command
cAbstractDir f fDir args ss = apply $ map (getPath ss) args
  where err = fail "Must provide unleast 2 arguments"
        apply []         = err
        apply [_]        = err
        apply (dst:srcs) = mapM (applyOne dst) srcs >> return ss
        applyOne dst src = do
        	isDir <- doesDirectoryExist src
        	when (not isDir) $ fail $ "Given path '" ++ src 
        	                       ++ "' isn't a directory or doesn't exist"
        	createDirectoryR dst
        	content <- getDirectoryContents src
        	forM_ content $ \p ->
        	  when (not $ p=="." || p=="..") $ func p
        	fDir src
        	  where func path = do
        	        	let source  = src </> path
        	        	let newDest = dst </> path
        	        	isDirSource <- doesDirectoryExist source
        	        	if (isDirSource)
        	        		then applyOne newDest source
        	        		else f source newDest
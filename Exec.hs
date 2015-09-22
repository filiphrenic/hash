module Exec (
	Command, VarTable, CommandTable, ScriptState(..), 
	runHashProgram, runTopLevel, getPath, emptyScriptState
	) where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad (when, (>>=))
import System.FilePath.Posix (isRelative, (</>))
import System.IO
import qualified Proba as P

import Expressions

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output   :: String
                               , wd       :: FilePath
                               , vartable :: VarTable
                               } deriving Show

emptyScriptState :: ScriptState
emptyScriptState = ScriptState "" "" M.empty

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command's execution.
runHashProgram :: CommandTable -> ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram _  ss []     = return ss
runHashProgram ct ss (t:ts) = mScript >>= rHP
	where rHP scs = runHashProgram ct scs ts -- run the rest of the expressions
	      mScript = runTopLevel ct ss t -- run one expression

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct ss t = case t of
	TLCmd (Cmd n as i o a) -> case (M.lookup name ct) of
						        	Nothing  -> fail $ "Undefined command " ++ name
						        	Just cmd -> resolveCmd cmd
		where name = eval n
		      resolveCmd :: Command -> IO ScriptState
		      resolveCmd cmd = args >>= cmd' >>= resolveScript
		        where cmd' = flip cmd ss
		              args = case i of
		              	Nothing -> return $ map eval as
		              	Just f  -> getInputFrom $ eval f
		      resolveScript :: ScriptState -> IO ScriptState
		      resolveScript ss = do
		      	let out = output ss
		      	case o of
		      		Nothing -> if null out then return () else putStrLn out
		      		Just f  -> toOutputFile a (eval f) out
		      	return ss { output = "" }
	TLCmt _                  -> return ss
	TLCnd (If c cts)         -> if (evalPred vt c) 
									then rHP cts
									else return ss
	TLCnd (IfElse c cts ces) -> if (evalPred vt c)
									then rHP cts
									else rHP ces
	TLCas (Assign var val)   -> rTL $ TLCmd
		(Cmd (Expr "assign") [var, val] Nothing Nothing False)
	w@(TLClp (While c b))    -> if (evalPred vt c)
	                           then rHP b >>= fRTL w 
	                           else return ss
	d@(TLClp (DoWhile c b))  -> do
		s <- rHP b
		if (evalPred (vartable s) c)
			then rTL' s d
			else return s
	(TLClp f@(For initial _ _ _))  -> rTL (TLCas initial) >>= runFor f
	where vt = vartable ss
	      eval = evalExpr vt
	      rHP' s = runHashProgram ct s . map TLCmd
	      rHP = rHP' ss
	      rTL' = runTopLevel ct
	      rTL = rTL' ss
	      fRTL = flip rTL'
	      runFor f@(For _ c s b) script = do
	      	if (evalPred (vartable script) c)
	      		then rHP' script b >>= fRTL (TLCas s) >>= runFor f
	      		else return script

getInputFrom :: FilePath -> IO [String]
getInputFrom path = lines <$> readFile path

toOutputFile :: Bool -> (FilePath -> String -> IO ())
toOutputFile append = if append then appendFile else writeFile

-- LOWER LEVEL FUNCTIONS ==========================================================

-- evaluates the given expression
-- for a string returns it, for a variable returns it's value
oldEvalExpr :: VarTable -> Expr -> String
oldEvalExpr vt (Expr s) = (unwords . map find . words) s
  where find ('$':xs) = case (M.lookup xs vt) of
        	Nothing -> '$':xs
        	Just v  -> v
        find xs       = xs


evalExpr :: VarTable -> Expr -> String
evalExpr vt e = case (evalExpr' vt e) of
  Just s  -> s
  Nothing -> oldEvalExpr vt e 

evalExpr' :: VarTable -> Expr -> Maybe String
evalExpr' vt (Expr s) = P.parseE s >>= P.eval vt


evalComp :: VarTable -> Comp -> Bool
evalComp vt c = case c of
	CEQ e1 e2 -> f (==) e1 e2
	CNE e1 e2 -> f (/=) e1 e2
	CGE e1 e2 -> f (>=) e1 e2
	CGT e1 e2 -> f (>)  e1 e2
	CLE e1 e2 -> f (<=) e1 e2
	CLT e1 e2 -> f (<)  e1 e2
	CLI e     -> not . null $ ev e
	where f c e1 e2 = c (ev e1) (ev e2)
	      ev e = evalExpr vt e

evalPred :: VarTable -> Pred -> Bool
evalPred vt p = case p of
	Pred c    -> evalComp vt c
	Not  p    -> not $ evalPred vt p
	And p1 p2 -> evalPred vt p1 && evalPred vt p2
	Or  p1 p2 -> evalPred vt p1 || evalPred vt p2

-- for given wd in scriptstate makes absolute path from given path
getPath :: ScriptState -> FilePath -> FilePath
getPath sc path
  | isRelative path = wd sc </> path
  |	otherwise       = path
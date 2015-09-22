module Expressions (
	Expr(..), Cmd(..), Assign(..), Comp(..), 
  Pred(..), Conditional(..), Loop(..), Comment(..),
  TLExpr(..)
	) where

-- Contains the data structures describing the structure of the language itself

-- A bottom-level expression; just a wrapper
data Expr = Expr String -- A named variable or a mere string
            deriving (Eq, Show)

-- A command which performs something - can be a command that takes arguments
-- or an assignmen.
data Cmd = Cmd { name   :: Expr       -- The command name (can be a variable)
               , args   :: [Expr]     -- The command arguments
               , inDir  :: Maybe Expr -- A redirected input fp
               , outDir :: Maybe Expr -- A redirected output fp
               , append :: Bool       -- If redirected, is it appending?
               } deriving Show

data Assign = Assign { var :: Expr -- Assignment target
                     , val :: Expr -- A value to assign to a variable
                     } deriving Show

data Comment = Comment String deriving Show

-- A comparison operation
data Comp = CEQ Expr Expr -- ==
          | CNE Expr Expr -- /=
          | CGE Expr Expr -- >=
          | CGT Expr Expr -- >
          | CLE Expr Expr -- <=
          | CLT Expr Expr -- <
          | CLI Expr      -- A wrapped expression literal - True if nonempty
            deriving (Eq, Show)

-- Something that evaluates to a truth value
data Pred = Pred Comp     -- A wrapped comparison
          | Not Pred      -- Negation
          | And Pred Pred -- A binary logical and
          | Or Pred Pred  -- A binary logical or
            deriving (Eq, Show)

-- A conditional branching expression - if-then or if-then-else
-- If-then with a condition and a list of actions
data Conditional = If { cond :: Pred -- Predicate to satisfy
                      , cthen :: [Cmd] -- Actions if satisfied
                      }
-- An if-then-else with a condition and two possible paths
                 | IfElse { cond :: Pred -- Predicate to satisfy
                          , cthen :: [Cmd] -- Actions if satisfied
                          , celse :: [Cmd] -- Actions otherwise
                          }
                   deriving Show

data Loop = While { condl :: Pred  -- predicate to satisfy
                  , body  :: [Cmd] -- body of the loop
                  }
          | DoWhile { condl :: Pred
                    , body  :: [Cmd]
                    }
          | For { initial :: Assign
                , condl   :: Pred
                , step    :: Assign -- only supports i=i+1, etc
                , body    :: [Cmd]
                } deriving Show

-- A top-level expression, wrapping either a conditional expression or a
-- command
data TLExpr = TLCmd Cmd
            | TLCnd Conditional
            | TLCas Assign
            | TLClp Loop
            | TLCmt Comment
              deriving Show

-- step u for petlji nije dobar
-- TODOOO
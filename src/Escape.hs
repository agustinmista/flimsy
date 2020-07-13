module Escape
  ( module Escape
  , SCC(..)
  ) where

import Control.Monad.Reader

import Data.Graph

import Data.Set (Set)
import qualified Data.Set as Set

import Var
import Syntax
import Error

----------------------------------------
-- | Dependency analysis
----------------------------------------

calculateSSCs :: [PsDecl] -> [SCC (PsDecl, Var, [Var])]
calculateSSCs decls = stronglyConnCompR (node <$> decls)

tryTopSort :: [PsDecl] -> Either FlimsyError [PsDecl]
tryTopSort decls = foldSCCs sccs
  where
    sccs = stronglyConnComp (node <$> decls)
    declVar (BindD bind) = let (_,v,_) = splitBind bind in v

    foldSCCs []                  = Right []
    foldSCCs (CyclicSCC vs : _)  = Left (CyclicDeclarations (declVar <$> vs))
    foldSCCs (AcyclicSCC v : xs) = (v:) <$> foldSCCs xs

node :: PsDecl -> (PsDecl, Var, [Var])
node (BindD bind) = (BindD bind, var, Set.toList escaped)
  where
    (_, var, expr) = splitBind bind
    escaped = escapedVars expr

----------------------------------------
-- | Escape analysis
----------------------------------------

type Scope = Reader (Set Var)

escapedVars :: PsExpr -> Set Var
escapedVars expr = runReader (escapedExpr expr) Set.empty

escapedExpr :: PsExpr -> Scope (Set Var)
escapedExpr expr = do
  case expr of
    -- VarE
    VarE var -> do
      scope <- ask
      if Set.member var scope
        then return Set.empty
        else return (Set.singleton var)
    -- AppE
    AppE e1 e2 -> do
      es1 <- escapedExpr e1
      es2 <- escapedExpr e2
      return (es1 <> es2)
    -- LamE
    LamE x e -> do
      local (Set.insert x) (escapedExpr e)
    -- LetE
    LetE b e2 -> do
      let (_, var, e1) = splitBind b
      ese1 <- escapedExpr e1
      ese2 <- local (Set.insert var) (escapedExpr e2)
      return (ese1 <> ese2)
    -- LitE
    LitE _ -> return Set.empty
    -- InfixE
    InfixE op e1 e2 -> do
      escapedExpr (AppE (AppE (VarE op) e1) e2)
    -- IfE
    IfE cond tr fl -> do
      escond <- escapedExpr cond
      estr <- escapedExpr tr
      esfl <- escapedExpr fl
      return (escond <> estr <> esfl)
    -- CaseE
    CaseE e alts -> do
      ese <- escapedExpr e
      esalts <- mapM escapedAlt alts
      return (ese <> Set.unions esalts)
    -- FixE
    FixE e -> do
      escapedExpr e
    -- TupE
    TupE es -> do
      eses <- mapM escapedExpr es
      return (Set.unions eses)
    -- SumE
    SumE (Left e) -> do
      escapedExpr e
    SumE (Right e) -> do
      escapedExpr e
    -- ListE
    ListE xs -> do
      esxs <- mapM escapedExpr xs
      return (Set.unions esxs)
    -- DoE
    DoE stmts -> do
      escapedDo stmts

escapedAlt :: PsAlt -> Scope (Set Var)
escapedAlt (Alt pat body) =
  local (Set.fromList (patVars pat) <>) (escapedExpr body)

escapedDo :: [PsDoStmt] -> Scope (Set Var)
escapedDo [] = return Set.empty
escapedDo (BindStmt v e : xs) = do
  ese <- escapedExpr e
  esxs <- local (Set.insert v) (escapedDo xs)
  return (ese <> esxs)
escapedDo (ExprStmt e : xs) = do
  ese <- escapedExpr e
  esxs <- escapedDo xs
  return (ese <> esxs)

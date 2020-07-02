module Escape where

import Control.Monad.Reader

import Data.Graph

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Env as Env

import Syntax
import Error
import Infer

----------------------------------------
-- | Dependency analysis
----------------------------------------

tryTopSort :: TcEnv -> [Decl] -> Either EscapeError [Decl]
tryTopSort env decls = foldSCCs sccs
  where
    envVars = Set.fromList (Env.keys env)
    sccs = stronglyConnComp (node envVars <$> decls)
    declVar (BindD bind) = let (v,_,_) = getBind bind in v


    foldSCCs [] =
      Right []
    foldSCCs (CyclicSCC vs : _) =
      Left (CyclicDeclarations (declVar <$> vs))
    foldSCCs (AcyclicSCC v : xs) = do
      vs <- foldSCCs xs
      return (v : vs)

node :: Set Var -> Decl -> (Decl, Var, [Var])
node vs (BindD bind) = (BindD bind, var, Set.toList escaped)
  where
    (var, expr, _) = getBind bind
    escaped = escapedVars vs expr

----------------------------------------
-- | Escape analysis
----------------------------------------

type Scope = Reader (Set Var)

escapedVars :: Set Var -> Expr -> Set Var
escapedVars env expr = runReader (escapedExpr expr) env

escapedExpr :: Expr -> Scope (Set Var)
escapedExpr expr = do
  case expr of
    -- VarE
    VarE x -> do
      scope <- ask
      if Set.member x scope
        then return Set.empty
        else return (Set.singleton x)
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
      let (var, e1, _) = getBind b
      ese1 <- escapedExpr e1
      ese2 <- local (Set.insert var) (escapedExpr e2)
      return (ese1 <> ese2)
    -- LitE
    LitE (IntL _) -> return Set.empty
    LitE (DoubleL _) -> return Set.empty
    LitE (StringL _) -> return Set.empty
    LitE (BoolL _) -> return Set.empty
    LitE (CharL _) -> return Set.empty
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

escapedAlt :: Alt -> Scope (Set Var)
escapedAlt (Alt pat body) = local (Set.fromList pv <>) (escapedExpr body)
  where
    pv = patVars pat

escapedDo :: [DoStmt] -> Scope (Set Var)
escapedDo [] = return Set.empty
escapedDo (BindStmt v e : xs) = do
  ese <- escapedExpr e
  esxs <- local (Set.insert v) (escapedDo xs)
  return (ese <> esxs)
escapedDo (ExprStmt e : xs) = do
  ese <- escapedExpr e
  esxs <- escapedDo xs
  return (ese <> esxs)

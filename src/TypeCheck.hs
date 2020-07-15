module TypeCheck where

import Data.List
import Prelude hiding (mod)
import Control.Monad.Except

import qualified Env as Env
import Var
import Syntax
import Type
import Escape
import Infer
import Subst
import Solve
import Error
import Util

----------------------------------------
-- | Type checking full modules
----------------------------------------

typeCheckModule :: TcEnv -> PsModule -> Either FlimsyError (TcModule, [SCC (TcDecl, [Var], Scheme)])
typeCheckModule env mod = do
  let decls = module_decls mod
  let sccs = calculateSSCs decls
  sccs' <- typeCheckSCCs env sccs
  let decls' = concatMap unSCC sccs'
  let mod' = mod { module_decls = decls' }
  return (mod', sccs')

unSCC :: SCC (TcDecl, [Var], Scheme) -> [TcDecl]
unSCC (AcyclicSCC decl) = [fst3 decl]
unSCC (CyclicSCC decls) = fmap fst3 decls

----------------------------------------
-- | type checking strongly connected components
----------------------------------------

typeCheckSCCs :: TcEnv -> [SCC (PsDecl, Var, [Var])] -> Either FlimsyError [SCC (TcDecl, [Var], Scheme)]
typeCheckSCCs _   [] = return []
typeCheckSCCs env (scc:sccs) = do
  (scc', env') <- typeCheckSCC env scc
  sccs' <- typeCheckSCCs env' sccs
  return (scc':sccs')

typeCheckSCC :: TcEnv -> SCC (PsDecl, Var, [Var]) -> Either FlimsyError (SCC (TcDecl, [Var], Scheme), TcEnv)
typeCheckSCC env (AcyclicSCC (BindD bind, var, escaped)) = do
  checkDuplicate env var
  (bind', tsc) <- typeCheckBind env bind
  let env' = env `Env.extend` (var, tsc)
  return (AcyclicSCC (BindD bind', escaped, tsc), env')
typeCheckSCC env (CyclicSCC decls) = do
  let names = snd3 <$> decls
  mapM_ (checkDuplicate env) names
  bindInfers <- runInfer env $ do
    tvs <- mapM (const freshTVar) names
    let tscs = zip names (Forall [] <$> tvs)
    let withMutRecInScope m = foldr inExtEnv m tscs
    forM decls $ \(BindD bind, _, escaped) -> do
      (ty, cs, bind') <- withMutRecInScope (inferBind bind)
      return (bind', cs, escaped, ty)
  let (binds', css, escapeds, tys) = unzip4 bindInfers
  case runSolve (concat css) of
    Left err -> throwError err
    Right subst -> do
      let decls' = fmap (\bind -> BindD (fmap (fmap (apply subst)) bind)) binds'
      let tscs = fmap (closeOver . apply subst) tys
      let env' = foldl Env.extend env (zip names tscs)
      return (CyclicSCC (zip3 decls' escapeds tscs), env')

----------------------------------------
-- | type checking binds
----------------------------------------

typeCheckBind :: TcEnv -> PsBind -> Either FlimsyError (TcBind, Scheme)
typeCheckBind env bind = do
  let (isVal, var, expr) = splitBind bind
  checkDuplicate env var
  (tsc, expr') <- typeCheckExpr env expr
  let bind' = mergeBind isVal (var, instantiate' tsc) expr'
  return (bind', tsc)

----------------------------------------
-- | type checking expressions
----------------------------------------

typeCheckExpr :: TcEnv -> PsExpr -> Either FlimsyError (Scheme, TcExpr)
typeCheckExpr env expr = do
  (ty, cs, expr') <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (closeOver (apply subst ty), fmap (apply subst) <$> expr')

constraintsOfExpr :: TcEnv -> PsExpr -> Either FlimsyError ([Constraint], Subst, Type, Scheme)
constraintsOfExpr env expr = do
  (ty, cs, _) <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (cs, subst, ty, closeOver (apply subst ty))

----------------------------------------
-- | report duplicated top-level binds
----------------------------------------

checkDuplicate :: TcEnv -> Var -> Either FlimsyError ()
checkDuplicate env var = do
  case Env.lookup var env of
    Nothing -> return ()
    Just _ -> throwError (DuplicatedDecls var)

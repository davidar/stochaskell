module Data.Expression.Extract where

import Control.Monad.State
import qualified Data.Array as A
import Data.Expression
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Util

substEEnv :: EEnv -> EEnv
substEEnv env = Map.map (substD env) `fixpt` env

subst :: EEnv -> Expr e -> Expr e
subst env = Expr . substD env . erase

substD :: EEnv -> DExpr -> DExpr
substD env e = DExpr $ extractNodeRef simplifyNodeRef simplify env block ret
  where (ret, block) = runDExpr e

reExpr :: EEnv -> Block -> NodeRef -> Expr t
reExpr env block = Expr . reDExpr env block
reDExpr :: EEnv -> Block -> NodeRef -> DExpr
reDExpr env block = DExpr . extractNodeRef simplifyNodeRef simplify env block

extractNodeRef :: (NodeRef -> State Block NodeRef) -> (Node -> State Block NodeRef)
               -> EEnv -> Block -> NodeRef -> State Block NodeRef
extractNodeRef fNodeRef fNode env block = go where
  go r = case r of
    Var i@Internal{} _->
      extractNode $ lookupBlock i block
    Var i t
      | isJust val -> fromDExpr $ fromJust val
      | Volatile{} <- i -> do
        t' <- extractType env block t
        _ <- simplify $ Apply "getExternal" [Var i t'] t'
        return $ Var i t'
      | Dummy{} <- i -> do
        t' <- extractType env block t
        return $ Var i t'
      | Symbol{} <- i -> do
        t' <- extractType env block t
        return $ Var i t'
      where val = Map.lookup (LVar i) env
    Const c t -> do
      t' <- extractType env block t
      return $ Const c t'
    Data c i t -> do
      i' <- sequence $ go <$> i
      t' <- extractType env block t
      return $ Data c i' t'
    BlockArray a t -> do
      a' <- sequence $ go <$> a
      t' <- extractType env block t
      fNodeRef $ BlockArray a' t'
    Index v i -> do
      v' <- go v
      i' <- sequence $ go <$> i
      fNodeRef $ Index v' i'
    Extract d c i -> do
      d' <- go d
      fNodeRef $ case d' of
        Data k js _ | c == k, i < length js -> js !! i
        _ -> Extract d' c i
    Cond cvs t -> do
      let (cs,vs) = unzip cvs
      cs' <- sequence $ go <$> cs
      vs' <- sequence $ go <$> vs
      t' <- extractType env block t
      fNodeRef $ Cond (zip cs' vs') t'
    Unconstrained t -> do
      t' <- extractType env block t
      return $ Unconstrained t'
    PartiallyConstrained{} -> return r

  extractNode :: Node -> State Block NodeRef
  extractNode n = case n of
    Apply f args t -> do
      js <- sequence $ go <$> args
      t' <- extractType env block t
      fNode $ Apply f js t'
    Array sh lam t -> do
      let (lo,hi) = unzip sh
      lo' <- sequence $ go <$> lo
      hi' <- sequence $ go <$> hi
      let sh' = zip lo' hi'
      lam' <- extractLambda lam
      t' <- extractType env block t
      dag <- topDAG <$> get
      if varies dag (map fst sh' ++ map snd sh') || variesLambda dag lam'
      then fNode $ Array sh' lam' t'
      else liftBlock $ extractNode n
    FoldScan fs lr lam sd ls t -> do
      lam' <- extractLambda lam
      sd' <- go sd
      ls' <- go ls
      t' <- extractType env block t
      dag <- topDAG <$> get
      if varies dag [sd',ls'] || variesLambda dag lam'
      then fNode $ FoldScan fs lr lam' sd' ls' t'
      else liftBlock $ extractNode n
    Case hd lams t -> do
      hd' <- go hd
      lams' <- sequence $ extractLambda' <$> lams
      t' <- extractType env block t
      dag <- topDAG <$> get
      if varies dag [hd'] || variesLambda' dag `any` lams'
      then fNode $ Case hd' lams' t'
      else liftBlock $ extractNode n
    Function lam t -> do
      lam' <- extractLambda lam
      t' <- extractType env block t
      dag <- topDAG <$> get
      if variesLambda dag lam'
      then fNode $ Function lam' t'
      else liftBlock $ extractNode n

  extractLambda :: Lambda NodeRef -> State Block (Lambda NodeRef)
  extractLambda (Lambda body hd) = do
    Lambda dag [ret] <- extractLambda' $ Lambda body [hd]
    return $ Lambda dag ret

  extractLambda' :: Lambda [NodeRef] -> State Block (Lambda [NodeRef])
  extractLambda' (Lambda body hds) =
    runLambda (inputsT body) (sequence $ extractNodeRef fNodeRef fNode env' block' <$> hds)
    where f (i,t) = DExpr . return $ Var i t
          ids' = f <$> inputsT body
          env' = Map.fromList (inputsL body `zip` ids') `Map.union` env
          block' = deriveBlock body block

extractType :: EEnv -> Block -> Type -> State Block Type
extractType env block = go where
  go ty = case ty of
    IntT -> return IntT
    RealT -> return RealT
    SubrangeT t a b -> do
      t' <- go t
      a' <- f a
      b' <- f b
      return $ SubrangeT t' a' b'
    ArrayT k sh t -> do
      lo' <- f lo
      hi' <- f hi
      t' <- go t
      return $ ArrayT k (zip lo' hi') t'
      where (lo,hi) = unzip sh
    TupleT ts -> do
      ts' <- sequence $ go <$> ts
      return $ TupleT ts'
    UnionT tss -> do
      tss' <- sequence $ sequence . map go <$> tss
      return $ UnionT tss'
    UnknownType -> return UnknownType
  f :: (Traversable t) => t NodeRef -> State Block (t NodeRef)
  f t = sequence $ extractNodeRef simplifyNodeRef simplify env block <$> t

optimiseE :: Expr t -> Expr t
optimiseE = Expr . optimiseD . erase

optimiseD :: DExpr -> DExpr
optimiseD e = DExpr $ extractNodeRef optimiseNodeRef simplify emptyEEnv block ret
  where (ret, block) = runDExpr e

flattenCond :: (NodeRef,NodeRef) -> State Block [(NodeRef,NodeRef)]
flattenCond (Cond cas _, Cond cbs _) = do
  let (cs,vs) = unzip [(simplifyConj [c,c'], b)
                      | (c,a) <- cas, getConstVal a == Just 1, (c',b) <- cbs]
  cs' <- sequence cs
  return (zip cs' vs)
flattenCond (Cond cas _, b) =
  return [(c,b) | (c,a) <- cas, getConstVal a == Just 1]
flattenCond (c, Cond cbs _) = do
  let (cs,vs) = unzip [(simplifyConj [c,c'], b) | (c',b) <- cbs]
  cs' <- sequence cs
  return (zip cs' vs)
flattenCond cv = return [cv]

optimiseNodeRef :: NodeRef -> State Block NodeRef
optimiseNodeRef (BlockArray a t) = do
  a' <- sequence $ optimiseNodeRef <$> a
  if isCond `all` A.elems a' && replicated (getConds <$> A.elems a') then
    let cvs = do
          c <- unreplicate (getConds <$> A.elems a')
          let f (Cond cvs' _) = fromJust $ lookup c cvs'
          return (c, BlockArray (f <$> a') t)
    in optimiseNodeRef $ Cond cvs t
  else simplifyNodeRef $ BlockArray a' t
optimiseNodeRef (Cond cvs t) | isCond `any` (map fst cvs ++ map snd cvs) = do
  cvs' <- sequence $ flattenCond <$> cvs
  optimiseNodeRef $ Cond (concat cvs') t
optimiseNodeRef ref = simplifyNodeRef ref

extractIndex :: DExpr -> [DExpr] -> DExpr
extractIndex e = DExpr . (extractIndexNode emptyEEnv block $ lookupBlock r block)
  where (Var r _, block) = runDExpr e

extractIndexNode :: EEnv -> Block -> Node -> [DExpr] -> State Block NodeRef
extractIndexNode env block (Array _ (Lambda body hd) _) idx =
  extractNodeRef simplifyNodeRef simplify env' block' hd
  where env' = Map.fromList (inputsL body `zip` idx) `Map.union` env
        block' = deriveBlock body block
extractIndexNode env block (Apply op [a,b] t) idx
  | op `elem` ["+","-","*","/"] = do
  a' <- extractIndexNodeRef env block a idx
  b' <- extractIndexNodeRef env block b idx
  t' <- extractType env block $ typeIndex (length idx) t
  simplify $ Apply op [a',b'] t'
extractIndexNode env block (Apply "ifThenElse" [c,a,b] t) idx = do
  c' <- extractNodeRef simplifyNodeRef simplify env block c
  a' <- extractIndexNodeRef env block a idx
  b' <- extractIndexNodeRef env block b idx
  t' <- extractType env block $ typeIndex (length idx) t
  simplify $ Apply "ifThenElse" [c',a',b'] t'
extractIndexNode _ _ n idx = error $ "extractIndexNode "++ show n ++" "++ show idx

extractIndexNodeRef :: EEnv -> Block -> NodeRef -> [DExpr] -> State Block NodeRef
extractIndexNodeRef env block r _ | not . isArrayT $ typeRef r =
  extractNodeRef simplifyNodeRef simplify env block r
extractIndexNodeRef env block (Var i@Internal{} _) idx =
  extractIndexNode env block (lookupBlock i block) idx
extractIndexNodeRef _ _ r idx = error $ "extractIndexNodeRef "++ show r ++" "++ show idx

collapseArray :: DExpr -> DExpr
collapseArray e | (ArrayT _ [(lo,hi)] (ArrayT _ [(lo',hi')] t)) <- typeRef ret = DExpr $ do
  los <- sequence $ extractNodeRef simplifyNodeRef simplify emptyEEnv block <$> [lo,lo']
  his <- sequence $ extractNodeRef simplifyNodeRef simplify emptyEEnv block <$> [hi,hi']
  t' <- extractType emptyEEnv block t
  d <- getNextLevel
  let sh = zip los his
      v = (e `extractIndex` [DExpr . return $ Var (Dummy (-1) 1) IntT])
             `extractIndex` [DExpr . return $ Var (Dummy (-1) 2) IntT]
      env' = Map.fromList [(LVar (Dummy (-1) 1), DExpr . return $ Var (Dummy d 1) IntT)
                          ,(LVar (Dummy (-1) 2), DExpr . return $ Var (Dummy d 2) IntT)]
  lam <- runLambda [(Dummy d 1,IntT),(Dummy d 2,IntT)] (fromDExpr $ substD env' v)
  hashcons $ Array sh lam (ArrayT Nothing sh t')
  where (ret, block) = runDExpr e
collapseArray e = substD emptyEEnv e

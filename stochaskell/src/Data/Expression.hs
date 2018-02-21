{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, ConstraintKinds #-}
module Data.Expression where

import Prelude hiding (const,foldl,foldr,scanl,scanr)

import qualified Data.Array.Abstract as AA
import Data.Array.Abstract ((!))
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Char
import Data.Expression.Const
import Data.List hiding (foldl,foldr,scanl,scanr)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Number.Transfinite hiding (log)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Control.Applicative ()
import Control.Monad.State
import qualified Numeric.LinearAlgebra as LA
import Util


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Int
type Level = Int

data Id = Dummy    { idLevel :: Level, idPointer :: Pointer }
        | Volatile { idLevel :: Level, idPointer :: Pointer }
        | Internal { idLevel :: Level, idPointer :: Pointer }
        deriving (Eq, Ord)

isInternal :: Id -> Bool
isInternal (Internal _ _) = True
isInternal _ = False

instance Show Id where
  show (Dummy l p)    = "i_"++ show l ++"_"++ show p
  show (Volatile l p) = "x_"++ show l ++"_"++ show p
  show (Internal l p) = "v_"++ show l ++"_"++ show p

data NodeRef = Var Id Type
             | Const ConstVal Type
             | Index NodeRef [NodeRef]
             | Extract NodeRef Int
             deriving (Eq, Ord)

getId :: NodeRef -> Maybe Id
getId (Var i _) = Just i
getId _ = Nothing

getConstVal :: NodeRef -> Maybe ConstVal
getConstVal (Const c _) = Just c
getConstVal _ = Nothing

instance Show NodeRef where
  show (Var   i t) = "("++ show i ++" :: "++ show t ++")"
  show (Const c IntT) = show (integer c)
  show (Const c RealT) = show (real c)
  show (Const c t) = "("++ show c ++" :: "++ show t ++")"
  show (Index f js) = intercalate "!" (show f : map show (reverse js))
  show (Extract v i) = show v ++"."++ show i

data Lambda h = Lambda { fDefs :: DAG, fHead :: h } deriving (Eq, Ord)

data FoldOrScan = Fold | Scan | ScanRest deriving (Eq, Ord, Show)
data LeftRight = Left_ | Right_ deriving (Eq, Ord, Show)

data Node = Apply { fName :: String
                  , fArgs :: [NodeRef]
                  , typeNode :: Type
                  }
          | Array { aShape :: [AA.Interval NodeRef]
                  , aFunc  :: Lambda NodeRef
                  , typeNode :: Type
                  }
          | FoldScan
                  { rScan :: FoldOrScan
                  , rDirection :: LeftRight
                  , rFunc :: Lambda NodeRef
                  , rSeed :: NodeRef
                  , rList :: NodeRef
                  , typeNode :: Type
                  }
          deriving (Eq, Ord)
showLet :: (Show r) => DAG -> r -> String
showLet dag ret = showLet' dag $ show ret
showLet' :: DAG -> String -> String
showLet' dag ret
  | show dag == "" = ret
  | otherwise = "let "++ indent' (show dag) ++"\n"++
                " in "++ indent' ret
  where indent' = drop 4 . indent . indent
instance Show Node where
  show (Apply f args t)
    | all (not . isAlphaNum) f, [i,j] <- args
    = show i ++" "++ f ++" "++ show j ++" :: "++ show t
    | otherwise = f ++" "++ intercalate " " (map show args) ++" :: "++ show t
  show (Array sh (Lambda dag hd) t) = "\n"++
    "  [ "++ (drop 4 . indent . indent $ showLet dag hd) ++"\n"++
    "  | "++ intercalate ", " (zipWith g (inputs dag) sh) ++" ] :: "++ show t
    where g i (a,b) = show i ++" <- "++ show a ++"..."++ show b
  show (FoldScan fs lr (Lambda dag hd) seed ls _) =
    name ++" "++ show seed ++" "++ show ls ++" $ "++
    "\\"++ show i ++" "++ show j ++" ->\n"++
      indent (showLet dag hd)
    where name = case (fs,lr) of
            (Fold, Right_) -> "foldr"
            (Fold, Left_)  -> "foldl"
            (Scan, Right_) -> "scanr"
            (Scan, Left_)  -> "scanl"
            (ScanRest, Left_) -> "scan"
          [i,j] = case lr of
            Right_ -> inputs dag
            Left_ -> reverse $ inputs dag

data DAG = DAG { dagLevel :: Level
               , inputs :: [Id]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord)
emptyDAG :: DAG
emptyDAG = DAG 0 [] Bimap.empty
nodes :: DAG -> [(Pointer, Node)]
nodes dag = Bimap.toAscListR $ bimap dag
instance Show DAG where
  show dag = unlines $ map f (nodes dag)
    where f (i,n) = show (Internal (dagLevel dag) i) ++" = "++ show n

newtype Block = Block [DAG] deriving (Eq, Ord)
emptyBlock :: Block
emptyBlock = Block [emptyDAG]

topDAG :: Block -> DAG
topDAG (Block ds) = head ds

nextLevel :: Block -> Level
nextLevel (Block ds) = length ds

lookupBlock :: Id -> Block -> Node
lookupBlock i@(Internal level ptr) (Block dags) =
  fromMaybe (error $ "internal lookup failure: " ++ show i) $
    ptr `lookup` nodes (reverse dags !! level)

deriveBlock :: DAG -> Block -> Block
deriveBlock d b@(Block ds) = Block (d:parent)
  where parent = drop (nextLevel b - dagLevel d) ds

showBlock :: [DAG] -> String -> String
showBlock (dag:block) r = showBlock block $ showLet' dag r
showBlock [] r = r

data DExpr = DExpr { fromDExpr :: State Block NodeRef }
runDExpr :: DExpr -> (NodeRef, Block)
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where
  e == f = runDExpr e == runDExpr f
instance Ord DExpr where
  e `compare` f = runDExpr e `compare` runDExpr f
instance Show DExpr where
  show e = showBlock block $ show ret
    where (ret, Block block) = runDExpr e

type EEnv = Map Id DExpr
emptyEEnv :: EEnv
emptyEEnv = Map.empty

type Expression t = Expr t
newtype Expr t = Expr { erase :: DExpr }
expr :: State Block NodeRef -> Expr t
expr = Expr . DExpr
fromExpr :: Expr t -> State Block NodeRef
fromExpr = fromDExpr . erase
runExpr :: Expr t -> (NodeRef, Block)
runExpr  =  runDExpr . erase
instance (Eq t) => Eq (Expr t) where
  e == f = runExpr e == runExpr f
instance (Ord t) => Ord (Expr t) where
  e `compare` f = runExpr e `compare` runExpr f

type B = Expression Bool
type R = Expression Double
type Z = Expression Integer
type BVec = Expr [Bool]
type RVec = Expr [Double]
type ZVec = Expr [Integer]
type BMat = Expr [[Bool]]
type RMat = Expr [[Double]]
type ZMat = Expr [[Integer]]


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

data Type
    = IntT
    | RealT
    | SubrangeT Type (Maybe NodeRef) (Maybe NodeRef)
    -- TODO: better encoding of constraints than (Maybe String)
    | ArrayT (Maybe String) [AA.Interval NodeRef] Type
    | TupleT [Type]
    deriving (Eq, Ord)
boolT :: Type
boolT = SubrangeT IntT (Just $ Const 0 IntT) (Just $ Const 1 IntT)

instance Show Type where
  show IntT = "Z"
  show RealT = "R"
  show (SubrangeT IntT (Just (Const 0 IntT)) (Just (Const 1 IntT))) = "B"
  show (SubrangeT t a b) = unwords ["Subrange", show t, show a, show b]
  show (ArrayT Nothing sh t) = unwords ["Array", show sh, show t]
  show (ArrayT (Just name) sh t) = unwords [name, show sh, show t]
  show (TupleT ts) = show ts

isArrayT :: Type -> Bool
isArrayT ArrayT{} = True
isArrayT _ = False

newtype TypeOf t = TypeIs Type
class ScalarType t where
    typeOf :: TypeOf t
instance ScalarType Integer where
    typeOf = TypeIs IntT
instance ScalarType Bool where
    typeOf = TypeIs boolT
instance ScalarType Double where
    typeOf = TypeIs RealT
instance forall t. (ScalarType t) => ScalarType [t] where
    typeOf = TypeIs t
      where TypeIs t = typeOf :: TypeOf t

internal :: Level -> Pointer -> State Block NodeRef
internal level i = do
    t <- getType
    return $ Var (Internal level i) t
  where getType = do
          block <- get
          let dag = topDAG block
          if level == dagLevel dag
            then return . typeNode . fromJust . Bimap.lookupR i $ bimap dag
            else liftBlock getType

typeDExpr :: DExpr -> Type
typeDExpr = typeRef . fst . runDExpr

typeRef :: NodeRef -> Type
typeRef (Var _ t) = t
typeRef (Const _ t) = t
typeRef (Index a js) = case typeRef a of
  (ArrayT k sh t) | length js == length sh -> t
                  | otherwise -> ArrayT k (drop (length js) sh) t
  t -> error $ "cannot index non-array type "++ show t

typeArray :: ([Integer],[Integer]) -> Type -> Type
typeArray ([],[]) = id
typeArray (lo,hi) = ArrayT Nothing $ zip (f lo) (f hi)
  where f = map $ flip Const IntT . fromInteger

typeDims :: Type -> [(NodeRef,NodeRef)]
typeDims IntT = []
typeDims RealT = []
typeDims (SubrangeT t _ _) = typeDims t
typeDims (ArrayT _ d _) = d

typeIndex :: Type -> Type
typeIndex (ArrayT _ [_] t) = t
typeIndex (ArrayT (Just "matrix") (_:sh) t) = ArrayT (Just "row_vector") sh t
typeIndex (ArrayT _ (_:sh) t) = ArrayT Nothing sh t
typeIndex t = error $ "cannot index objects of type "++ show t

coerce :: Type -> Type -> Type
coerce a b | a == b = a
coerce a b | (a == boolT && b == IntT) || (a == IntT && b == boolT) = IntT
coerce IntT RealT = RealT
coerce RealT IntT = RealT
coerce a@(ArrayT _ _ t) t' | t == t' = a
coerce t a@(ArrayT _ _ t') | t == t' = a
coerce (ArrayT n sh t) (ArrayT n' sh' t') | n == n' && t == t' =
  ArrayT n (AA.coerceShape sh sh') t
coerce s t = error $ "cannot coerce "++ show s ++" with "++ show t

cast :: Expr a -> Expr b
cast = expr . fromExpr


------------------------------------------------------------------------------
-- DAG BUILDING                                                             --
------------------------------------------------------------------------------

-- http://okmij.org/ftp/tagless-final/sharing/sharing.pdf
hashcons :: Node -> State Block NodeRef
hashcons e = do
    block <- get
    let (DAG level inp vdag) = topDAG block
    case Bimap.lookup e vdag of
        Just k -> internal level k
        Nothing -> do
            let k = Bimap.size vdag
            put $ deriveBlock (DAG level inp $ Bimap.insert e k vdag) block
            internal level k

-- perform an action in the enclosing block
liftBlock :: MonadState Block m => State Block b -> m b
liftBlock s = do
    Block (dag:parent) <- get
    let (ref, parent') = runState s $ Block parent
    put $ deriveBlock dag parent'
    return ref

-- does a list of expressions depend on the inputs to this block?
varies :: DAG -> [NodeRef] -> Bool
varies (DAG level inp _) xs = level == 0 || any p xs
  where p (Var (Internal l _) _) = l == level
        p (Var (Volatile l _) _) = l == level
        p (Var s _) = s `elem` inp
        p (Const _ _) = False
        p (Index a is) = p a || any p is

-- collect External references
externRefs :: DAG -> [Id]
externRefs (DAG _ _ d) = go d
  where go defs = concatMap (f . snd) $ Bimap.toAscListR defs
        f (Apply _ args _) = mapMaybe extern args
        f (Array sh (Lambda (DAG _ _ defs') r) _) =
            mapMaybe extern [r] ++ mapMaybe (extern . fst) sh
                                ++ mapMaybe (extern . snd) sh ++ go defs'
        f FoldScan{} = [] -- TODO
        extern (Var (Internal _ _) _) = Nothing
        extern (Var i _) = Just i
        extern _ = Nothing
        -- TODO Index

-- recursive data dependencies
dependsNodeRef :: Block -> NodeRef -> Set Id
dependsNodeRef block (Var i@Internal{} _) =
  Set.insert i . dependsNode block $ lookupBlock i block
dependsNodeRef _ (Var i _) = Set.singleton i
dependsNodeRef _ (Const _ _) = Set.empty
dependsNodeRef block (Index a i) =
  Set.unions $ map (dependsNodeRef block) (a:i)

dependsNode :: Block -> Node -> Set Id
dependsNode block (Apply _ args _) =
  Set.unions $ map (dependsNodeRef block) args
dependsNode block (Array sh (Lambda body hd) _) =
  Set.unions $ map (d . fst) sh ++ map (d . snd) sh ++ [hdeps]
  where d = dependsNodeRef block
        hdeps = Set.filter ((dagLevel body >) . idLevel) $
          dependsNodeRef (deriveBlock body block) hd

substD :: EEnv -> DExpr -> DExpr
substD env e = DExpr $ extractNodeRef env block ret
  where (ret, block) = runDExpr e

extractNodeRef :: EEnv -> Block -> NodeRef -> State Block NodeRef
extractNodeRef env block (Var i@Internal{} _) = do
  node' <- extractNode env block $ lookupBlock i block
  simplify node'
extractNodeRef env block (Var i t)
  | isJust val = fromDExpr $ fromJust val
  | otherwise = do
    t' <- extractType env block t
    return $ Var i t'
  where val = Map.lookup i env
extractNodeRef env block (Const c t) = do
  t' <- extractType env block t
  return $ Const c t'
extractNodeRef env block (Index v i) = do
  v' <- extractNodeRef env block v
  i' <- sequence $ extractNodeRef env block <$> i
  return $ Index v' i'
extractNodeRef _ _ r = error $ "extractNodeRef "++ show r

extractNode :: EEnv -> Block -> Node -> State Block Node
extractNode env block (Apply f args t) = do
  js <- sequence $ extractNodeRef env block <$> args
  t' <- extractType env block t
  return $ Apply f js t'
extractNode env block (Array sh (Lambda body hd) t) = do
  lo' <- sequence $ extractNodeRef env block <$> lo
  hi' <- sequence $ extractNodeRef env block <$> hi
  t' <- extractType env block t
  newBlock <- get
  let d = nextLevel newBlock
      ids = [ Dummy d i | i <- [1..length sh] ]
      ids' = DExpr . return . flip Var IntT <$> ids
      env' = Map.fromList (inputs body `zip` ids') `Map.union` env
      (ret, Block (dag:newBlock')) = runState (extractNodeRef env' block' hd) $
        deriveBlock (DAG d ids Bimap.empty) newBlock
  put $ Block newBlock'
  return $ Array (zip lo' hi') (Lambda dag ret) t'
  where block' = deriveBlock body block
        (lo,hi) = unzip sh
extractNode _ _ n = error $ show n

extractType :: EEnv -> Block -> Type -> State Block Type
extractType _ _ IntT = return IntT
extractType _ _ RealT = return RealT
extractType env block (SubrangeT t a b) = do
  t' <- extractType env block t
  a' <- sequence $ extractNodeRef env block <$> a
  b' <- sequence $ extractNodeRef env block <$> b
  return $ SubrangeT t' a' b'
extractType env block (ArrayT k sh t) = do
  lo' <- sequence $ extractNodeRef env block <$> lo
  hi' <- sequence $ extractNodeRef env block <$> hi
  t' <- extractType env block t
  return $ ArrayT k (zip lo' hi') t'
  where (lo,hi) = unzip sh

extractIndex :: DExpr -> [DExpr] -> DExpr
extractIndex e = DExpr . (extractIndexNode emptyEEnv block $ lookupBlock r block)
  where (Var r _, block) = runDExpr e

extractIndexNode :: EEnv -> Block -> Node -> [DExpr] -> State Block NodeRef
extractIndexNode env block (Array _ (Lambda body hd) _) idx =
  extractNodeRef env' block' hd
  where env' = Map.fromList (inputs body `zip` idx) `Map.union` env
        block' = deriveBlock body block
extractIndexNode env block (Apply op [a,b] t) idx
  | op `elem` ["+","-","*","/"] = do
  a' <- extractIndexNodeRef env block a idx
  b' <- extractIndexNodeRef env block b idx
  t' <- extractType env block $ typeIndex t -- TODO: broken for length sh > 1
  simplify $ Apply op [a',b'] t'
extractIndexNode env block (Apply "ifThenElse" [c,a,b] t) idx = do
  c' <- extractNodeRef env block c
  a' <- extractIndexNodeRef env block a idx
  b' <- extractIndexNodeRef env block b idx
  t' <- extractType env block $ typeIndex t
  simplify $ Apply "ifThenElse" [c',a',b'] t'
extractIndexNode _ _ n idx = error $ "extractIndexNode "++ show n ++" "++ show idx

extractIndexNodeRef :: EEnv -> Block -> NodeRef -> [DExpr] -> State Block NodeRef
extractIndexNodeRef env block r _ | not . isArrayT $ typeRef r =
  extractNodeRef env block r
extractIndexNodeRef env block (Var i@Internal{} _) idx =
  extractIndexNode env block (lookupBlock i block) idx
extractIndexNodeRef _ _ r idx = error $ "extractIndexNodeRef "++ show r ++" "++ show idx

collapseArray :: DExpr -> DExpr
collapseArray e | (ArrayT _ [(lo,hi)] (ArrayT _ [(lo',hi')] t)) <- typeRef ret = DExpr $ do
  los <- sequence $ extractNodeRef emptyEEnv block <$> [lo,lo']
  his <- sequence $ extractNodeRef emptyEEnv block <$> [hi,hi']
  t' <- extractType emptyEEnv block t
  newBlock <- get
  let sh = zip los his
      d = nextLevel newBlock
      v = (e `extractIndex` [DExpr . return $ Var (Dummy (-1) 1) IntT])
             `extractIndex` [DExpr . return $ Var (Dummy (-1) 2) IntT]
      env' = Map.fromList [(Dummy (-1) 1, DExpr . return $ Var (Dummy d 1) IntT)
                          ,(Dummy (-1) 2, DExpr . return $ Var (Dummy d 2) IntT)]
      (ret', Block (dag:newBlock')) = runState (fromDExpr $ substD env' v) $
        deriveBlock (DAG d [Dummy d 1,Dummy d 2] Bimap.empty) newBlock
  put $ Block newBlock'
  hashcons $ Array sh (Lambda dag ret') (ArrayT Nothing sh t')
  where (ret, block) = runDExpr e
collapseArray e = substD emptyEEnv e


------------------------------------------------------------------------------
-- FUNCTION APPLICATION                                                     --
------------------------------------------------------------------------------

-- simplify and float constants
simplify :: Node -> State Block NodeRef
simplify (Apply "ifThenElse" [_,a,b] _) | a == b = return a
simplify (Apply "ifThenElse" [_,Const a _,Const b _] t)
  | a == b = return $ Const a t
simplify (Apply "*" [Const c _,_] t) | c == 0 = return $ Const 0 t
simplify (Apply "*" [_,Const c _] t) | c == 0 = return $ Const 0 t
simplify (Apply "*" [Const c _,x] _) | c == 1 = return x
simplify (Apply "*" [x,Const c _] _) | c == 1 = return x
simplify (Apply "+" [Const c _,x] _) | c == 0 = return x
simplify (Apply "+" [x,Const c _] _) | c == 0 = return x
simplify (Apply "-" [x,Const c _] _) | c == 0 = return x
simplify (Apply "*" [Const c _,x] t)
  | c == -1 = simplify (Apply "negate" [x] t)
simplify (Apply "*" [x,Const c _] t)
  | c == -1 = simplify (Apply "negate" [x] t)
simplify (Apply "-" [Const c _,x] t) | c == 0 = simplify (Apply "negate" [x] t)
simplify (Apply "==" [x,y] t) | x == y = return $ Const 1 t
simplify (Apply f args t)
  | Just f' <- Map.lookup f constFuns
  , Just args' <- sequence (getConstVal <$> args)
  = return $ Const (f' args') t
simplify e@(Apply _ args _) = do
  mr <- simplify' e
  block <- get
  case mr of
    Just r -> return r
    Nothing -> if varies (topDAG block) args
      then hashcons e
      else liftBlock $ simplify e
simplify e = hashcons e

-- TODO: generalise to something less hacky
simplify' :: Node -> State Block (Maybe NodeRef)
simplify' (Apply "==" [Var i@Internal{} _,x] t) = do
  block <- get
  case lookupBlock i block of
    (Apply "-" [y,Const c _] _) | x == y, c /= 0 -> return . Just $ Const 0 t
    _ -> return Nothing
simplify' _ = return Nothing

apply :: String -> Type -> [ Expr a ] -> Expr r
apply f t xs = expr $ do
    js <- mapM fromExpr xs
    simplify $ Apply f js t

apply1 :: String -> Type -> Expr a -> Expr r
apply1 f t x = apply f t [x]

apply2 :: String -> Type -> Expr a -> Expr b -> Expr r
apply2 f t x y = Expr $ apply2' f t (erase x) (erase y)
apply2' :: String -> Type -> DExpr -> DExpr -> DExpr
apply2' f t x y = DExpr $ do
    i <- fromDExpr x
    j <- fromDExpr y
    simplify $ Apply f [i,j] t

applyClosed1 :: String -> Expr a -> Expr a
applyClosed1 f x = expr $ do
    i <- fromExpr x
    simplify $ Apply f [i] (typeRef i)

applyClosed2 :: String -> Expr a -> Expr a -> Expr a
applyClosed2 f x y = Expr $ applyClosed2' f (erase x) (erase y)
applyClosed2' :: String -> DExpr -> DExpr -> DExpr
applyClosed2' f x y = DExpr $ do
    i <- fromDExpr x
    j <- fromDExpr y
    let s = typeRef i
        t = typeRef j
    simplify $ Apply f [i,j] (coerce s t)


------------------------------------------------------------------------------
-- ARRAY COMPREHENSIONS                                                     --
------------------------------------------------------------------------------

-- External references captured by this closure
capture :: AA.AbstractArray DExpr DExpr -> [Id]
capture ar = externRefs $ topDAG block
  where (_,block) = runDExpr $ ar ! replicate (length $ AA.shape ar) x
        x = DExpr . return $ Const (error "capture dummy") IntT

makeArray :: Maybe String -> AA.AbstractArray DExpr DExpr
          -> [AA.Interval NodeRef] -> State Block NodeRef
makeArray l ar sh = do
    block <- get
    let d = nextLevel block
        ids = [ Dummy d i | i <- [1..length sh] ]
        e = ar ! [ DExpr . return $ Var i IntT | i <- ids ]
        (ret, Block (dag:block')) = runState (fromDExpr e) $
            deriveBlock (DAG d ids Bimap.empty) block
        t = typeDExpr e
    put $ Block block'
    hashcons $ Array sh (Lambda dag ret) (ArrayT l sh t)

-- constant floating
floatArray :: Maybe String -> AA.AbstractArray DExpr DExpr
           -> State Block NodeRef
floatArray l ar = do
    block <- get
    let dag = topDAG block
    sh <- sequence . flip map (AA.shape ar) $ \interval -> do
        i <- fromDExpr $ fst interval
        j <- fromDExpr $ snd interval
        return (i,j)
    if varies dag (map fst sh) ||
       varies dag (map snd sh) ||
       (not . null $ inputs dag `intersect` capture ar)
      then makeArray l ar sh
      else liftBlock $ floatArray l ar

-- TODO: reduce code duplication
floatArray' :: Node -> State Block NodeRef
floatArray' a@(Array sh (Lambda adag _) _) = do
    block <- get
    let dag = topDAG block
    if varies dag (map fst sh) ||
       varies dag (map snd sh) ||
       (not . null $ inputs dag `intersect` externRefs adag)
      then hashcons a
      else liftBlock $ floatArray' a

array' :: Maybe String -> Int -> AA.AbstractArray DExpr DExpr -> DExpr
array' l n a = if length sh == n
                then DExpr $ floatArray l a
                else error "dimension mismatch"
  where sh = AA.shape a
array :: (Num i, ScalarType i, ScalarType e)
      => Maybe String -> Int -> AA.AbstractArray (Expr i) (Expr e) -> Expr t
array l n = Expr . array' l n . eraseAA

eraseAA :: AA.AbstractArray (Expr i) (Expr e) -> AA.AbstractArray DExpr DExpr
eraseAA (AA.AArr sh f) = AA.AArr sh' f'
  where sh' = flip map sh $ \(a,b) -> (erase a, erase b)
        f' = erase . f . map Expr

index :: Expr a -> [Expr i] -> Expr r
index a es = expr $ do
    f <- fromExpr a
    js <- mapM fromExpr es
    return $ Index f js

foldl :: (ScalarType b) =>
  (Expr b -> Expr a -> Expr b) -> Expr b -> Expr [a] -> Expr b
foldl f r xs = expr $ foldscan Fold Left_ (flip f) r xs

foldr :: (ScalarType b) =>
  (Expr a -> Expr b -> Expr b) -> Expr b -> Expr [a] -> Expr b
foldr f r xs = expr $ foldscan Fold Right_ f r xs

scanl :: (ScalarType b) =>
  (Expr b -> Expr a -> Expr b) -> Expr b -> Expr [a] -> Expr [b]
scanl f r xs = expr $ foldscan Scan Left_ (flip f) r xs

scanr :: (ScalarType b) =>
  (Expr a -> Expr b -> Expr b) -> Expr b -> Expr [a] -> Expr [b]
scanr f r xs = expr $ foldscan Scan Right_ f r xs

scan :: (ScalarType b) =>
  (Expr b -> Expr a -> Expr b) -> Expr b -> Expr [a] -> Expr [b]
scan f r xs = expr $ foldscan ScanRest Left_ (flip f) r xs

foldscan :: forall a b. (ScalarType b) => FoldOrScan -> LeftRight ->
  (Expr a -> Expr b -> Expr b) -> Expr b -> Expr [a] -> State Block NodeRef
foldscan fs dir f r xs = do
    seed <- fromExpr r
    l <- fromExpr xs
    block <- get
    let d = nextLevel block
        (ArrayT _ [(Const 1 IntT,n)] _) = typeRef l
        s = typeIndex $ typeRef l
        TypeIs t = typeOf :: TypeOf b
        i = expr . return $ Var (Dummy d 1) s
        j = expr . return $ Var (Dummy d 2) t
        (ret, Block (dag:block')) = runState (fromExpr $ f i j) $
          deriveBlock (DAG d [Dummy d 1, Dummy d 2] Bimap.empty) block
    if varies (topDAG block) [seed,l] ||
       (not . null $ inputs (topDAG block) `intersect` externRefs dag)
      then do
        put $ Block block'
        case fs of
          Fold ->
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t
          Scan -> do
            n1 <- simplify $ Apply "+" [n, Const 1 IntT] IntT
            let t' = (ArrayT Nothing [(Const 1 IntT, n1)] t)
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t'
          ScanRest -> do
            let t' = (ArrayT Nothing [(Const 1 IntT, n)] t)
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t'
      else liftBlock $ foldscan fs dir f r xs


------------------------------------------------------------------------------
-- INSTANCES                                                                --
------------------------------------------------------------------------------

instance Num DExpr where
    (+) = applyClosed2' "+"
    (-) = applyClosed2' "-"
    (*) = applyClosed2' "*"
    fromInteger x = trace ("WARN assuming IntT type for DExpr "++ show x) $
      DExpr . return $ Const (fromInteger x) IntT
instance Fractional DExpr where
    (/) = applyClosed2' "/"
instance Floating DExpr where
    (**) = applyClosed2' "**"

instance (ScalarType t, Num t) => Num (Expr t) where
    (+) = applyClosed2 "+"
    (-) = applyClosed2 "-"
    (*) = applyClosed2 "*"

    negate = applyClosed1 "negate"
    abs    = applyClosed1 "abs"
    signum = applyClosed1 "signum"

    fromInteger  = expr . return . flip Const t . fromInteger
      where TypeIs t = typeOf :: TypeOf t

instance (ScalarType t, Fractional t) => Fractional (Expr t) where
    fromRational = expr . return . flip Const t . fromRational
      where TypeIs t = typeOf :: TypeOf t
    (/) = applyClosed2 "/"

instance (ScalarType t, Floating t) => Floating (Expr t) where
    pi = apply "pi" RealT []
    (**) = applyClosed2 "**"

    exp   = applyClosed1 "exp"
    log   = applyClosed1 "log"
    sqrt  = applyClosed1 "sqrt"
    sin   = applyClosed1 "sin"
    cos   = applyClosed1 "cos"
    tan   = applyClosed1 "tan"
    asin  = applyClosed1 "asin"
    acos  = applyClosed1 "acos"
    atan  = applyClosed1 "atan"
    sinh  = applyClosed1 "sinh"
    cosh  = applyClosed1 "cosh"
    tanh  = applyClosed1 "tanh"
    asinh = applyClosed1 "asinh"
    acosh = applyClosed1 "acosh"
    atanh = applyClosed1 "atanh"

instance AA.Indexable DExpr DExpr DExpr where
    a ! e = DExpr $ do
        f <- fromDExpr a
        j <- fromDExpr e
        return $ case f of
            (Index g js) -> Index g (j:js)
            _            -> Index f [j]
instance AA.Vector DExpr DExpr DExpr where
    vector = array' (Just "vector") 1

instance AA.Indexable (Expr [e]) (Expr Integer) (Expr e) where
    a ! e = Expr $ erase a ! erase e
    deleteIndex a e = expr $ do
      f <- fromExpr a
      j <- fromExpr e
      let (ArrayT _ [(lo,hi)] t) = typeRef f
      hi' <- simplify $ Apply "-" [hi, Const 1 IntT] (typeRef hi)
      let t' = ArrayT Nothing [(lo,hi')] t
      simplify $ Apply "deleteIndex" [f,j] t'
    insertIndex a e d = expr $ do
      f <- fromExpr a
      i <- fromExpr e
      j <- fromExpr d
      let (ArrayT _ [(lo,hi)] t) = typeRef f
      hi' <- simplify $ Apply "+" [hi, Const 1 IntT] (typeRef hi)
      let t' = ArrayT Nothing [(lo,hi')] t
      simplify $ Apply "insertIndex" [f,i,j] t'
instance (ScalarType e) => AA.Vector (Expr [e]) (Expr Integer) (Expr e) where
    vector = array (Just "vector") 1
instance (ScalarType e) => AA.Matrix (Expr [[e]]) (Expr Integer) (Expr e) where
    matrix = array (Just "matrix") 2
instance (ScalarType e) => Monoid (Expr [[e]]) where
    mappend a b = expr $ do
        i <- fromExpr a
        j <- fromExpr b
        let (ArrayT _ [r,_] t) = typeRef i
            (ArrayT _ [_,c] _) = typeRef j
        simplify $ Apply "<>" [i,j] (ArrayT (Just "matrix") [r,c] t)

instance AA.Scalable R RVec where
    a *> v = expr $ do
        i <- fromExpr a
        j <- fromExpr v
        simplify $ Apply "*>" [i,j] (typeRef j)
instance (ScalarType e) => AA.Scalable (Expr e) (Expr [[e]]) where
    a *> m = expr $ do
        i <- fromExpr a
        j <- fromExpr m
        simplify $ Apply "*>" [i,j] (typeRef j)

instance (ScalarType e) => LA.Transposable (Expr [[e]]) (Expr [[e]]) where
    tr m = expr $ do
        i <- fromExpr m
        let (ArrayT _ [r,c] t) = typeRef i
        simplify $ Apply "tr" [i] (ArrayT (Just "matrix") [c,r] t)
    tr' m = expr $ do
        i <- fromExpr m
        let (ArrayT _ [r,c] t) = typeRef i
        simplify $ Apply "tr'" [i] (ArrayT (Just "matrix") [c,r] t)

instance AA.InnerProduct (Expr [e]) (Expr e) where
    u <.> v = expr $ do
        i <- fromExpr u
        j <- fromExpr v
        let (ArrayT _ _ t) = typeRef i
        simplify $ Apply "<.>" [i,j] t

instance AA.LinearOperator (Expr [[e]]) (Expr [e]) where
    m #> v = expr $ do
        i <- fromExpr m
        j <- fromExpr v
        let (ArrayT _ [r,_] t) = typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)
    diag v = expr $ do
        i <- fromExpr v
        let (ArrayT _ [n] t) = typeRef i
        simplify $ Apply "diag" [i] (ArrayT (Just "matrix") [n,n] t)
    asColumn v = expr $ do
        i <- fromExpr v
        let (ArrayT _ [n] t) = typeRef i
        simplify $ Apply "asColumn" [i] (ArrayT (Just "matrix") [n,(Const 1 IntT, Const 1 IntT)] t)
    asRow v = expr $ do
        i <- fromExpr v
        let (ArrayT _ [n] t) = typeRef i
        simplify $ Apply "asRow"    [i] (ArrayT (Just "matrix") [(Const 1 IntT, Const 1 IntT),n] t)

instance AA.SquareMatrix (Expr [[e]]) (Expr e) where
    chol m = expr $ do
        i <- fromExpr m
        let (ArrayT _ sh t) = typeRef i
        simplify $ Apply "chol" [i] (ArrayT (Just "matrix") sh t)
    inv m = expr $ do
        i <- fromExpr m
        let (ArrayT _ sh t) = typeRef i
        simplify $ Apply "inv" [i] (ArrayT (Just "matrix") sh t)
    det m = expr $ do
        i <- fromExpr m
        let (ArrayT _ _ t) = typeRef i
        simplify $ Apply "det" [i] t

qfDiag :: RMat -> RVec -> RMat
qfDiag m v = expr $ do
  i <- fromExpr m
  j <- fromExpr v
  let (ArrayT _ sh t) = typeRef i
  simplify $ Apply "quad_form_diag" [i,j] (ArrayT Nothing sh t)

instance AA.Broadcastable (Expr e) (Expr e) (Expr e) where
    bsxfun op a b = op a b
instance AA.Broadcastable (Expr e) (Expr [e]) (Expr [e]) where
    bsxfun op a b = op (cast a) (cast b)
instance AA.Broadcastable (Expr [e]) (Expr e) (Expr [e]) where
    bsxfun op a b = op (cast a) (cast b)
instance AA.Broadcastable (Expr e) (Expr [[e]]) (Expr [[e]]) where
    bsxfun op a b = op (cast a) (cast b)
instance AA.Broadcastable (Expr [[e]]) (Expr e) (Expr [[e]]) where
    bsxfun op a b = op (cast a) (cast b)

instance Boolean DExpr
instance Boolean (Expr Bool) where
    true  = apply "true" boolT []
    false = apply "false" boolT []
    notB  = applyClosed1 "not"
    (&&*) = applyClosed2 "&&"
    (||*) = applyClosed2 "||"

type instance BooleanOf DExpr = DExpr
type instance BooleanOf (Expr t) = Expr Bool

instance IfB (Expr t) where
  ifB c x y = Expr $ ifB (erase c) (erase x) (erase y)
instance IfB DExpr where
  ifB c x y = DExpr $ do
    k <- fromDExpr c
    i <- fromDExpr x
    j <- fromDExpr y
    let s = typeRef i
        t = typeRef j
    simplify $ Apply "ifThenElse" [k,i,j] (coerce s t)

instance EqB DExpr where
    (==*) = apply2' "==" boolT
    (/=*) = apply2' "/=" boolT
instance EqB (Expr t) where
    (==*) = apply2 "==" boolT
    (/=*) = apply2 "/=" boolT

instance OrdB (Expr t) where
    (<*)  = apply2 "<"  boolT
    (<=*) = apply2 "<=" boolT
    (>=*) = apply2 ">=" boolT
    (>*)  = apply2 ">"  boolT

instance (Ord t, ScalarType t) => Transfinite (Expr t) where
    infinity = constExpr infinity

class ExprTuple t where
    fromExprTuple :: t -> [DExpr]
    toExprTuple :: [DExpr] -> t
    fromConstVals :: [ConstVal] -> t

zipExprTuple :: (ExprTuple t) => t -> t -> [(DExpr,DExpr)]
zipExprTuple s t = fromExprTuple s `zip` fromExprTuple t

constDExpr :: ConstVal -> Type -> DExpr
constDExpr c = DExpr . return . Const c

constExpr :: forall t. (ScalarType t) => ConstVal -> Expr t
constExpr c = expr . return $ Const c t'
  where (lo,hi) = AA.bounds c
        sh = map f lo `zip` map f hi
          where f = flip Const IntT . fromInteger
        TypeIs t = typeOf :: TypeOf t
        t' = if sh == [] then t else ArrayT Nothing sh t

const :: (ScalarType t) => ConstVal -> Expr t
const = constExpr

instance (ScalarType a) => ExprTuple (Expr a) where
    fromExprTuple (a) = [erase a]
    toExprTuple [a] = (Expr a)
    fromConstVals [a] = (const a)
instance (ScalarType a, ScalarType b) =>
         ExprTuple (Expr a, Expr b) where
    fromExprTuple (a,b) = [erase a, erase b]
    toExprTuple [a,b] = (Expr a, Expr b)
    fromConstVals [a,b] = (const a, const b)
instance (ScalarType a, ScalarType b, ScalarType c) =>
         ExprTuple (Expr a, Expr b, Expr c) where
    fromExprTuple (a,b,c) = [erase a, erase b, erase c]
    toExprTuple [a,b,c] = (Expr a, Expr b, Expr c)
    fromConstVals [a,b,c] = (const a, const b, const c)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d) where
    fromExprTuple (a,b,c,d) = [erase a, erase b, erase c, erase d]
    toExprTuple [a,b,c,d] = (Expr a, Expr b, Expr c, Expr d)
    fromConstVals [a,b,c,d] = (const a, const b, const c, const d)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e) where
    fromExprTuple (a,b,c,d,e) =
      [erase a, erase b, erase c, erase d, erase e]
    toExprTuple [a,b,c,d,e] =
      (Expr a, Expr b, Expr c, Expr d, Expr e)
    fromConstVals [a,b,c,d,e] =
      (const a, const b, const c, const d, const e)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f) where
    fromExprTuple (a,b,c,d,e,f) =
      [erase a, erase b, erase c, erase d, erase e, erase f]
    toExprTuple [a,b,c,d,e,f] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f)
    fromConstVals [a,b,c,d,e,f] =
      (const a, const b, const c, const d, const e, const f)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f, ScalarType g) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g) where
    fromExprTuple (a,b,c,d,e,f,g) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g]
    toExprTuple [a,b,c,d,e,f,g] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g)
    fromConstVals [a,b,c,d,e,f,g] =
      (const a, const b, const c, const d, const e, const f, const g)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f, ScalarType g, ScalarType h) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g, Expr h) where
    fromExprTuple (a,b,c,d,e,f,g,h) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g, erase h]
    toExprTuple [a,b,c,d,e,f,g,h] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g, Expr h)
    fromConstVals [a,b,c,d,e,f,g,h] =
      (const a, const b, const c, const d, const e, const f, const g, const h)

{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, TypeFamilies,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, ConstraintKinds, RankNTypes, TupleSections #-}
module Data.Expression where

import Prelude hiding (const,foldl,foldr,scanl,scanr)

import qualified Data.Array as A
import qualified Data.Array.Abstract as AA
import Data.Array.Abstract ((!))
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Char
import Data.Expression.Const hiding (isScalar)
import Data.List hiding (foldl,foldr,scanl,scanr)
import Data.List.Extra hiding (foldl,foldr,scanl,scanr)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Number.Transfinite hiding (log)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Utils
import Debug.Trace
import Control.Applicative ()
import Control.Monad.State
import GHC.Exts (fromList,toList)
import qualified Numeric.LinearAlgebra as LA
import Util


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Int
type Level = Int
type NS = String

data Id = Dummy    { idLevel' :: Level, idPointer :: Pointer }
        | Volatile { idNS :: NS, idLevel' :: Level, idPointer :: Pointer }
        | Internal { idLevel' :: Level, idPointer :: Pointer }
        | Symbol   { idName :: String, idKnown :: Bool }
        deriving (Eq, Ord)

idLevel :: Id -> Level
idLevel Symbol{} = -1
idLevel i = idLevel' i

isInternal :: Id -> Bool
isInternal (Internal _ _) = True
isInternal _ = False

isSymbol :: Id -> Bool
isSymbol Symbol{} = True
isSymbol _ = False

instance Show Id where
  show (Dummy l p)       = "i_"++ show l ++"_"++ show p
  show (Volatile ns l p) = "x_"++ ns ++"_"++ show l ++"_"++ show p
  show (Internal l p)    = "v_"++ show l ++"_"++ show p
  show (Symbol s known)  = (if known then toUpper else toLower) <$> s

data NodeRef = Var Id Type
             | Const ConstVal Type
             | Data Tag [NodeRef] Type
             | BlockArray (A.Array [Int] NodeRef) Type
             | Index NodeRef [NodeRef]
             | Extract NodeRef Tag Int
             -- conditions are assumed to be mutually exclusive (ie. unordered)
             | Cond [(NodeRef,NodeRef)] Type
             | Unconstrained Type
             | PartiallyConstrained [AA.Interval DExpr] [(Id,Type)] [([DExpr],DExpr)] Type
             deriving (Eq, Ord)

getId :: NodeRef -> Maybe Id
getId (Var i _) = Just i
getId _ = Nothing

isConst :: NodeRef -> Bool
isConst Const{} = True
isConst _ = False
getConstVal :: NodeRef -> Maybe ConstVal
getConstVal (Const c _) = Just c
getConstVal _ = Nothing

isBlockArray :: NodeRef -> Bool
isBlockArray BlockArray{} = True
isBlockArray _ = False

isBlockVector :: NodeRef -> Bool
isBlockVector (BlockArray a _) = length (AA.shape a) == 1
isBlockVector _ = False
fromBlockVector :: NodeRef -> [NodeRef]
fromBlockVector (BlockArray a _) = [a![i] | i <- [1..n]]
  where [(1,n)] = AA.shape a

isBlockMatrix :: NodeRef -> Bool
isBlockMatrix (BlockArray a _) = length (AA.shape a) == 2
isBlockMatrix _ = False
fromBlockMatrix :: NodeRef -> [[NodeRef]]
fromBlockMatrix (BlockArray a _) = [[a![i,j] | j <- [1..n]] | i <- [1..m]]
  where [(1,m),(1,n)] = AA.shape a

isCond :: NodeRef -> Bool
isCond Cond{} = True
isCond _ = False
getConds :: NodeRef -> [NodeRef]
getConds (Cond cvs _) = map fst cvs

isUnconstrained :: NodeRef -> Bool
isUnconstrained Unconstrained{} = True
isUnconstrained _ = False

unconstrained :: Type -> DExpr
unconstrained = DExpr . return . Unconstrained
unconstrainedLike :: [DExpr] -> DExpr
unconstrainedLike es = DExpr $ do
  ts <- sequence $ typeDExpr <$> es
  return . Unconstrained $ coerces ts

-- TODO: make polymorphic
symbolR :: String -> R
symbolR name = expr . return $ Var (Symbol name False) RealT
symbolRVec :: String -> RVec
symbolRVec name = expr . return $ Var (Symbol name False) (vecT RealT)

instance Show NodeRef where
  --show (Var i t) = "("++ show i ++" :: "++ show t ++")"
  show (Var i _) = show i
  show (Const c IntT) = show (integer c)
  show (Const c RealT) = show (real c)
  --show (Const c t) = "("++ show c ++" :: "++ show t ++")"
  show (Const c _) = show c
  show (Data c rs t) = "(C"++ show c ++ show rs ++" :: "++ show t ++")"
  show i | isBlockMatrix i = "block"++ show (fromBlockMatrix i)
  show (BlockArray a t) = "(block "++ show a ++" :: "++ show t ++")"
  show (Index f js) = intercalate "!" (show f : map show (reverse js))
  show (Extract v i j) = show v ++"."++ show i ++"_"++ show j
  show (Cond cvs _) = "{ "++ f `commas` cvs ++" }"
    where f (c,v) = show c ++" => "++ show v
  show (Unconstrained _) = "???"
  show (PartiallyConstrained sh ids kvs _) =
    "( "++ intercalate ", " [show `commas` k ++" -> "++ show v | (k,v) <- kvs]
    ++" | "++ showParams (map fst ids) sh ++" )"

data Lambda h = Lambda { fDefs :: DAG, fHead :: h } deriving (Eq, Ord, Show)

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
          | Case  { cHead :: NodeRef
                  , cAlts :: [Lambda [NodeRef]]
                  , typeNode :: Type
                  }
          | Function
                  { fFunc :: Lambda NodeRef
                  , typeNode :: Type
                  }
          deriving (Eq, Ord)

showLet :: (Show r) => DAG -> r -> String
showLet dag ret = showLet' dag $ show ret
showLet' :: DAG -> String -> String
showLet' dag ret
  | show dag == "" = ret
  | otherwise = "let "++ indent' 0 4 (show dag) ++"\n"++
                " in "++ indent' 0 4 ret

showParam i (a,b) = show i ++" <- "++ show a ++"..."++ show b
showParams is sh = intercalate ", " $ zipWith showParam is sh

instance Show Node where
  show (Apply f args t)
    | all (not . isAlphaNum) f, [i,j] <- args
    = show i ++" "++ f ++" "++ show j ++" :: "++ show t
    | otherwise = f ++" "++ intercalate " " (map show args) ++" :: "++ show t
  show (Array sh (Lambda dag hd) t) = "\n"++
    "  [ "++ (drop 4 . indent . indent $ showLet dag hd) ++"\n"++
    "  | "++ showParams (inputs dag) sh ++" ] :: "++ show t
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
            Right_ -> inputs' dag
            Left_ -> reverse $ inputs' dag
  show (Case e alts _) = "case "++ show e ++" of\n"++ indent cases
    where cases = unlines $ do
            (i, Lambda dag ret) <- zip [0..] alts
            let lhs | typeRef e == IntT = show (i+1)
                    | otherwise = "C"++ show i ++" "++ intercalate " " (map show $ inputs dag)
                rhs = indent (showLet dag ret)
            return $ lhs ++" ->\n"++ rhs
  show (Function (Lambda dag hd) _) =
    "\\"++ unwords (show <$> inputs' dag) ++" ->\n"++
      indent (showLet dag hd)

data DAG = DAG { dagLevel :: Level
               , inputsT :: [(Id,Type)]
               , bimap  :: Bimap.Bimap Node Pointer
               } deriving (Eq, Ord)
emptyDAG :: DAG
emptyDAG = DAG 0 [] Bimap.empty
nodes :: DAG -> [(Pointer, Node)]
nodes dag = Bimap.toAscListR $ bimap dag
inputs :: DAG -> [Id]
inputs = map fst . inputsT
inputsL :: DAG -> [LVal]
inputsL = map LVar . inputs
inputs' :: DAG -> [NodeRef]
inputs' = map (uncurry Var) . inputsT
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
lookupBlock i@(Internal level ptr) (Block dags)
  | level < length dags =
  fromMaybe (error $ "internal lookup failure: " ++ show i) $
    ptr `Bimap.lookupR` bimap (reverse dags !! level)
  | otherwise = error $ "trying to access level "++ show level ++
                        " but block only has "++ show (length dags) ++"\n"++
                        showBlock dags (show i)

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

fromDExprs :: [DExpr] -> State Block [NodeRef]
fromDExprs = sequence . map fromDExpr

data LVal = LVar Id
          | LSub Id [DExpr]
          | LField Id Type Tag Int
          | LCond LVal DExpr
          | LConstr DExpr
          deriving (Eq, Ord, Show)

getId' :: LVal -> Maybe Id
getId' (LVar i ) = Just i
getId' (LSub i _) = Just i
getId' (LField i _ _ _) = Just i
getId' (LCond l _) = getId' l
getId' LConstr{} = Nothing

newtype EEnv = EEnv (Map LVal DExpr)
emptyEEnv :: EEnv
emptyEEnv = EEnv Map.empty

instance Show EEnv where
  show (EEnv m) = unlines [show i ++" :=\n"++ indent (show v) | (LVar i, v) <- Map.toAscList m]

unionEEnv :: EEnv -> EEnv -> EEnv
unionEEnv (EEnv a) (EEnv b) = EEnv $ Map.union a b
unionsEEnv :: [EEnv] -> EEnv
unionsEEnv envs = EEnv $ Map.unions [env | EEnv env <- envs]

lookupEEnv :: Id -> EEnv -> Maybe DExpr
lookupEEnv k (EEnv m) = Map.lookup (LVar k) m
insertEEnv :: Id -> DExpr -> EEnv -> EEnv
insertEEnv k v (EEnv m) = EEnv $ Map.insert (LVar k) v m

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

-- TODO: remove NodeRef's from Type, make subranges static and
--       query array sizes elsewhere
data Type
    = IntT
    | RealT
    | SubrangeT Type (Maybe NodeRef) (Maybe NodeRef)
    -- TODO: better encoding of constraints than (Maybe String)
    | ArrayT (Maybe String) [AA.Interval NodeRef] Type
    | TupleT [Type]
    | UnionT [[Type]]
    | UnknownType
    deriving (Eq, Ord)
boolT :: Type
boolT = SubrangeT IntT (Just $ Const 0 IntT) (Just $ Const 1 IntT)

tupleT :: [Type] -> Type
tupleT [t] = t
tupleT ts = TupleT ts

vecT :: Type -> Type
vecT = ArrayT (Just "vector") [(Const 1 IntT, Unconstrained IntT)]

instance Show Type where
  show IntT = "Z"
  show RealT = "R"
  show (SubrangeT IntT (Just (Const 0 IntT)) (Just (Const 1 IntT))) = "B"
  show (SubrangeT t a b) = unwords ["Subrange", show t, show a, show b]
  show (ArrayT Nothing sh t) = unwords ["Array", show sh, show t]
  --show (ArrayT (Just name) sh t) = unwords [name, show sh, show t]
  show (ArrayT (Just name) _ _) = name
  show (TupleT ts) = show ts
  show (UnionT ts) = "Union"++ show ts
  show UnknownType = "???"

isScalar :: Type -> Bool
isScalar IntT = True
isScalar RealT = True
isScalar (SubrangeT t _ _) = isScalar t
isScalar _ = False

isScalarD :: DExpr -> Bool
isScalarD e = isScalar $ typeRef ret
  where (ret,_) = runDExpr e

isArrayT :: Type -> Bool
isArrayT ArrayT{} = True
isArrayT _ = False

newtype TypeOf t = TypeIs Type
class ScalarType t where
    typeOf       :: TypeOf t
    toConcrete   :: ConstVal -> t
    fromConcrete :: t -> Expr t
    constVal     :: t -> ConstVal
    constExpr    :: ConstVal -> Expr t
    constExpr    = fromConcrete . toConcrete
instance ScalarType Integer where
    typeOf       = TypeIs IntT
    toConcrete   = toInteger
    fromConcrete = fromInteger
    constVal     = fromInteger
    constExpr c  = expr . return $ Const c IntT
instance ScalarType Bool where
    typeOf       = TypeIs boolT
    toConcrete   = toBool
    fromConcrete b = if b then true else false
    constVal     b = if b then true else false
    constExpr c  = expr . return $ Const c boolT
instance ScalarType Double where
    typeOf       = TypeIs RealT
    toConcrete   = toDouble
    fromConcrete = fromRational . toRational
    constVal     = fromRational . toRational
    constExpr c  = expr . return $ Const c RealT
instance forall t. (ScalarType t) => ScalarType [t] where
    typeOf       = TypeIs t where TypeIs t = typeOf :: TypeOf t
    toConcrete   = map toConcrete . toList
    fromConcrete = constExpr . constVal
    constVal     = fromList . map constVal
    constExpr c  = expr . return . Const c $ ArrayT Nothing sh t
      where (lo,hi) = AA.bounds c
            f = flip Const IntT . fromInteger
            sh = map f lo `zip` map f hi
            TypeIs t = typeOf :: TypeOf t

newtype Tags t = Tags [Tag]
class ScalarType c => Constructor c where
  tags :: Tags c
  construct   :: (forall t. ScalarType t => a -> Expr t) -> Tag -> [a] -> c
  deconstruct :: (forall t. ScalarType t => Expr t -> a) -> c -> (Tag, [a])
  typeUnion :: c -> State Block Type

toConcreteC :: Constructor t => ConstVal -> t
toConcreteC (Tagged c args) = construct constExpr c args

fromConcreteC :: Constructor t => t -> Expr t
fromConcreteC m = expr $ do
  js <- sequence args
  t <- typeUnion m
  return $ Data c js t
  where (c, args) = deconstruct fromExpr m

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

extractD :: DExpr -> Tag -> Int -> DExpr
extractD e c j = DExpr $ do
  i <- fromDExpr e
  return $ case i of
    (Data c' args _) | c /= c' -> error "tag mismatch"
                     | j >= length args -> error "index too large"
                     | otherwise -> args !! j
    _ -> Extract i c j

typeExpr :: Expr t -> State Block Type
typeExpr = typeDExpr . erase

typeDExpr :: DExpr -> State Block Type
typeDExpr e = do
  i <- fromDExpr e
  return $ typeRef i

typeExprTuple :: (ExprTuple t) => t -> State Block [Type]
typeExprTuple = sequence . fmap typeDExpr . fromExprTuple

typeRef :: NodeRef -> Type
typeRef (Var _ t) = t
typeRef (Const _ t) = t
typeRef (Data _ _ t) = t
typeRef (BlockArray a t) = t
typeRef (Index a js) = case typeRef a of
  (ArrayT k sh t) | length js == length sh -> t
                  | otherwise -> ArrayT k (drop (length js) sh) t
  t -> error $ "cannot index non-array type "++ show t
typeRef (Extract v c k) | UnionT ts <- typeRef v = ts!!c!!k
typeRef (Extract v 0 k) | TupleT ts <- typeRef v = ts!!k
typeRef (Extract v c k) | UnknownType <- typeRef v = UnknownType
typeRef i@Extract{} = error $ "cannot extract invalid tag "++ show i
typeRef (Cond _ t) = t
typeRef (Unconstrained t) = t

typeArray :: ([Integer],[Integer]) -> Type -> Type
typeArray ([],[]) = id
typeArray (lo,hi) = ArrayT Nothing $ zip (f lo) (f hi)
  where f = map $ flip Const IntT . fromInteger

typeDims :: Type -> [(NodeRef,NodeRef)]
typeDims IntT = []
typeDims RealT = []
typeDims (SubrangeT t _ _) = typeDims t
typeDims (ArrayT _ d _) = d

typeIndex :: Int -> Type -> Type
typeIndex 1 (ArrayT _ [_] t) = t
typeIndex 1 (ArrayT (Just "matrix") (_:sh) t) = ArrayT (Just "row_vector") sh t
typeIndex 1 (ArrayT _ (_:sh) t) = ArrayT Nothing sh t
typeIndex 1 t = error $ "cannot index objects of type "++ show t
typeIndex n t = typeIndex 1 $ typeIndex (n-1) t

coerce :: Type -> Type -> Type
coerce t UnknownType = t
coerce UnknownType t = t
coerce a b | a == b = a
coerce a b | (a == boolT && b == IntT) || (a == IntT && b == boolT) = IntT
coerce IntT RealT = RealT
coerce RealT IntT = RealT
coerce (SubrangeT s _ _) t = coerce s t
coerce t (SubrangeT s _ _) = coerce s t
coerce a@(ArrayT _ _ t) t' | t == t' = a
coerce t a@(ArrayT _ _ t') | t == t' = a
coerce (ArrayT Nothing sh t) (ArrayT n sh' t')
  | t == t', Just sh'' <- AA.coerceShape sh sh' = ArrayT n sh'' t
coerce (ArrayT n sh t) (ArrayT Nothing sh' t')
  | t == t', Just sh'' <- AA.coerceShape sh sh' = ArrayT n sh'' t
coerce (ArrayT n sh t) (ArrayT n' sh' t')
  | n == n', t == t', Just sh'' <- AA.coerceShape sh sh' = ArrayT n sh'' t
coerce (ArrayT nm1 ((Const 1 IntT,m):sh1) s)
       (ArrayT nm2 ((Const 1 IntT,n):sh2) t)
  | nm1 == nm2 || isNothing nm1 || isNothing nm2, s == t =
  ArrayT nm ((Const 1 IntT,n'):sh) t
  where ArrayT _ sh _ = coerce (ArrayT nm1 sh1 s) (ArrayT nm2 sh2 t)
        n' = if m == n then n else
          trace "WARN coercing dynamic array" (Unconstrained IntT)
        nm = if isNothing nm1 then nm2 else nm1
coerce m@(ArrayT (Just "matrix") [(Const 1 IntT,n),_] s)
       v@(ArrayT (Just "vector") [(Const 1 IntT,k)] t)
  | n == k, s == t = m
coerce v@(ArrayT (Just "vector") _ _) m@(ArrayT (Just "matrix") _ _) = coerce m v
coerce s t = error $ "cannot coerce "++ show s ++" with "++ show t

coerces :: [Type] -> Type
coerces = foldr1 coerce

class    Cast a b                          where cast :: a -> b
instance Cast Z R                          where cast = expr . fromExpr
instance Cast Int R                        where cast = fromIntegral
instance Cast (Expr t) (Expr [t])          where cast = expr . fromExpr
instance Cast (Expr t) (Expr [[t]])        where cast = expr . fromExpr
instance (ScalarType t) => Cast t (Expr t) where cast = fromConcrete


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

runLambda :: [(Id,Type)] -> State Block r -> State Block (Lambda r)
runLambda ids s = do
  block <- get
  let d = nextLevel block
      (ret, Block (dag:block')) = runState s $
          deriveBlock (DAG d ids Bimap.empty) block
  put $ Block block'
  return (Lambda dag ret)

-- does a list of expressions depend on the inputs to this block?
varies :: DAG -> [NodeRef] -> Bool
varies (DAG level inp _) xs = level == 0 || any p xs
  where p (Var i@Internal{} _) = idLevel i == level
        p (Var i@Volatile{} _) = idLevel i == level
        p (Var i@Dummy{} _) = i `elem` map fst inp
        p (Var Symbol{} _) = False
        p (Const _ _) = False
        p (Data _ is _) = any p is
        p (BlockArray a _) = any p (A.elems a)
        p (Index a is) = p a || any p is
        p (Extract v _ _) = p v
        p (Cond cvs _) = any p (map fst cvs) || any p (map snd cvs)
        p (Unconstrained _) = False
        p PartiallyConstrained{} = False

variesLambda :: DAG -> Lambda NodeRef -> Bool
variesLambda dag (Lambda adag ret) = variesLambda' dag (Lambda adag [ret])
variesLambda' :: DAG -> Lambda [NodeRef] -> Bool
variesLambda' dag (Lambda adag rets) =
  varies dag (rets \\ inputs' adag) ||
  (not . null $ inputs dag `intersect` externRefs adag)

-- collect external references
externRefs :: DAG -> [Id]
externRefs dag = concatMap (f . snd) (nodes dag) \\ inputs dag
  where f (Apply _ args _) = mapMaybe extern args
        f (Array sh (Lambda dag' r) _) =
          mapMaybe extern (r : map fst sh ++ map snd sh) ++ externRefs dag'
        f (FoldScan _ _ (Lambda dag' r) sd ls _) =
          mapMaybe extern [r,sd,ls] ++ externRefs dag'
        f (Case hd alts _) =
          mapMaybe extern [hd] ++ concat [mapMaybe extern rs ++ externRefs dag'
                                         | Lambda dag' rs <- alts]
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
dependsNodeRef block (Extract r _ _) = dependsNodeRef block r
dependsNodeRef _ Unconstrained{} = Set.empty
dependsNodeRef _ r = error $ "dependsNodeRef "++ show r

dependsNode :: Block -> Node -> Set Id
dependsNode block (Apply _ args _) =
  Set.unions $ map (dependsNodeRef block) args
dependsNode block (Array sh (Lambda body hd) _) =
  Set.unions $ map (d . fst) sh ++ map (d . snd) sh ++ [hdeps]
  where d = dependsNodeRef block
        hdeps = Set.filter ((dagLevel body >) . idLevel) $
          dependsNodeRef (deriveBlock body block) hd
dependsNode block (FoldScan _ _ (Lambda body hd) seed ls _) =
  d seed `Set.union` d ls `Set.union` hdeps
  where d = dependsNodeRef block
        hdeps = Set.filter ((dagLevel body >) . idLevel) $
          dependsNodeRef (deriveBlock body block) hd
dependsNode _ n = error $ "dependsNode "++ show n

dependsD :: DExpr -> Set Id
dependsD e = Set.filter (not . isInternal) $ dependsNodeRef block ret
  where (ret, block) = runDExpr e

getNextLevel :: State Block Level
getNextLevel = do
  block <- get
  return (nextLevel block)

condD :: [(DExpr,DExpr)] -> DExpr
condD cvs = DExpr $ do
  is <- fromDExprs cs
  js <- fromDExprs vs
  return $ Cond [(i,j) | (i,j) <- zip is js, getConstVal i /= Just 0] $
    coerces $ typeRef <$> js
  where (cs,vs) = unzip cvs

condProduct :: [NodeRef] -> State Block [(NodeRef,[NodeRef])]
condProduct js
  | null `any` cs = error "empty conditions in condProduct"
  | otherwise = do
    cs' <- sequence $ simplifyConj <$> cs
    return $ zip cs' vs
  where (cs,vs) = unzip $ liftCond js
        liftCond ((Cond cvs _):rest) =
            [(c:cs, v:vs) | (c,v) <- cvs, (cs,vs) <- liftCond $ lookupCond c <$> rest]
        liftCond (a:rest) = [(cs, a:vs) | (cs,vs) <- liftCond rest]
        liftCond [] = [([],[])]
        lookupCond c (Cond cvs _) | Just v <- lookup c cvs = v
        lookupCond _ r = r


------------------------------------------------------------------------------
-- FUNCTION APPLICATION                                                     --
------------------------------------------------------------------------------

blockDot :: [NodeRef] -> [NodeRef] -> State Block NodeRef
blockDot u v = do
  w <- sequence [simplify . Apply "<>" [a,b] $ typeRef a `typeMatrixProduct` typeRef b
                | (a,b) <- zip u v]
  simplify $ Apply "+s" w (coerces $ typeRef <$> w)

simplifyNodeRef :: NodeRef -> State Block NodeRef
simplifyNodeRef (Cond cvs t) = return $ case mapMaybe f cvs of
  [] -> Unconstrained t
  cvs' -> Cond cvs' t
  where f (Const 0 _,_) = Nothing
        f (_,Unconstrained _) = Nothing
        f (c,v) | c == v = Just (c, Const 1 boolT)
        f cv = Just cv
simplifyNodeRef r = case r of
  Index i [Cond cvs _]     -> simplifyNodeRef $ Cond [(c,Index i [j])   | (c,j) <- cvs] (typeRef r)
  Index   (Cond cvs _) js  -> simplifyNodeRef $ Cond [(c,Index i js)    | (c,i) <- cvs] (typeRef r)
  Extract (Cond cvs _) k j -> simplifyNodeRef $ Cond [(c,Extract i k j) | (c,i) <- cvs] (typeRef r)
  _ -> return r

-- simplify and float constants
simplify :: Node -> State Block NodeRef
simplify (Apply "ifThenElse" [_,a,b] _) | a == b = return a
simplify (Apply "ifThenElse" [_,Const a _,Const b _] t)
  | a == b = return $ Const a t
simplify (Apply "ifThenElse" [Const 1 _,a,_] _) = return a
simplify (Apply "ifThenElse" [Const 0 _,_,b] _) = return b
simplify (Apply "ifThenElse" [Cond cvs _,a,b] t) | isConst `all` vs = do
  let vs' = [if c == 1 then a else b | Const c _ <- vs]
  r <- simplifyNodeRef $ Cond (zip cs vs') t
  simplify $ Apply "id" [r] t
  where (cs,vs) = unzip cvs
simplify (Apply f js t) | isCond `any` js, not $ f `elem` ["ifThenElse","id","log_det"] = do
  (cs,vs) <- unzip <$> condProduct js
  vs' <- sequence [simplify $ Apply f v t | v <- vs]
  r <- simplifyNodeRef $ Cond (zip cs vs') t
  case head js of
    Cond cvs _ | endswith "pdf" f -> do
      ns <- sequence [simplify $ Apply "not" [c] boolT | (c,_) <- cvs]
      c <- simplifyConj ns
      let z = if endswith "lpdf" f then 0 else 1
      simplify $ Apply "ifThenElse" [c, Const z t, r] t
    _ -> return r
simplify (Apply "*" [Const 0 _,_] t) = return $ Const 0 t
simplify (Apply "*" [_,Const 0 _] t) = return $ Const 0 t
simplify (Apply "*" [Const c _,x] t) | isZeros c, isScalar (typeRef x) = return $ Const c t
simplify (Apply "*" [x,Const c _] t) | isZeros c, isScalar (typeRef x) = return $ Const c t
simplify (Apply "/" [Const 0 _,_] t) = return $ Const 0 t
simplify (Apply "*" [Const 1 _,x] _) = return x
simplify (Apply "*" [x,Const 1 _] _) = return x
simplify (Apply "/" [x,Const 1 _] _) = return x
simplify (Apply "+" [Const 0 _,x] _) = return x
simplify (Apply "+" [x,Const 0 _] _) = return x
simplify (Apply "+" [Const c _,x] _) | isZeros c, not $ isScalar (typeRef x) = return x
simplify (Apply "+" [x,Const c _] _) | isZeros c, not $ isScalar (typeRef x) = return x
simplify (Apply "-" [x,Const 0 _] _) = return x
simplify (Apply "*" [Const (-1) _,x] t) = simplify (Apply "negate" [x] t)
simplify (Apply "*" [x,Const (-1) _] t) = simplify (Apply "negate" [x] t)
simplify (Apply "-" [Const 0 _,x] t) = simplify (Apply "negate" [x] t)
simplify (Apply f js t) | elem f ["+","+s","-","*","/","==","/=","<","<=",">",">="
                                 ,"exp","log","det","log_det"]
                        , isUnconstrained `any` js = return $ Unconstrained t
simplify (Apply "==" [x,y] t) | x == y = return $ Const 1 t
simplify (Apply "==" [x,y] t) | x > y = simplify (Apply "==" [y,x] t) -- sort by id
simplify (Apply "&&" js _) = simplifyConj js
simplify (Apply "&&s" js _) = simplifyConj js
simplify (Apply "||" [Const 0 _,x] _) = return x
simplify (Apply "||" [x,Const 0 _] _) = return x
simplify (Apply "||" [Const 1 _,_] t) = return $ Const 1 t
simplify (Apply "||" [_,Const 1 _] t) = return $ Const 1 t
simplify (Apply "#>" [_,Const 0 _] t) = return $ Const 0 t
simplify (Apply "<#" [Const 0 _,_] t) = return $ Const 0 t
simplify (Apply "det"     [i@BlockArray{}] t) | Just s <- simplifyDet False i t = s
simplify (Apply "log_det" [i@BlockArray{}] t) | Just s <- simplifyDet True  i t = s
simplify (Apply "replaceIndex" [Const 0 _,_,Const 0 _] t) = return $ Const 0 t
simplify (Apply "+s" [a,b] t) = simplify (Apply "+" [a,b] t)
simplify (Apply "+s" [a] _) = return a
simplify (Apply f args t)
  | Just f' <- Map.lookup f constFuns
  , Just args' <- sequence (getConstVal <$> args)
  = return $ Const (f' args') t
simplify (Case Unconstrained{} _ t) = return $ Unconstrained t
simplify (Case (Const c IntT) alts _)
  | Lambda dag [ret] <- alts !! (integer c - 1), null (nodes dag) = return ret
  | Lambda dag rets <- alts !! (integer c - 1), null (nodes dag) =
      return $ Data 0 rets (TupleT $ typeRef <$> rets)
simplify (Case (Cond cvs _) alts t) = do
  let (cs,vs) = unzip cvs
  vs' <- sequence [simplify $ Case v alts t | v <- vs]
  return $ Cond (zip cs vs') t
--simplify n@Array{}    = floatNode n
--simplify n@FoldScan{} = floatNode n
--simplify n@Case{}     = floatNode n
simplify f = do
  block <- get
  case simplify' block f of
    Right r -> r
    Left e -> liftcons e

liftcons :: Node -> State Block NodeRef
liftcons e@(Apply _ args _) = do
  block <- get
  if varies (topDAG block) args
  then hashcons e
  else liftBlock $ simplify e
liftcons e = hashcons e

-- TODO: generalise to something less hacky
simplify' :: Block -> Node -> Either Node (State Block NodeRef)
simplify' block (Apply "==" [Var i@Internal{} _,x] t)
  | (Apply "-" [y,Const c _] _) <- lookupBlock i block, x == y, c /= 0
  = Right . return $ Const 0 t
simplify' block (Apply "*" [x,Var i@Internal{} _] _)
  | (Apply "/" [j,y] _) <- lookupBlock i block, x == y = Right $ return j
simplify' block (Apply "/" [Var i@Internal{} _,x] _)
  | (Apply "*" [y,j] _) <- lookupBlock i block, x == y = Right $ return j
simplify' block (Apply "/" [Const c s,Var i@Internal{} _] t)
  | (Apply "/" [j,Const d s'] _) <- lookupBlock i block
  = Left $ Apply "/" [Const (c*d) (coerce s s'),j] t
simplify' block (Apply "-" [Var i@Internal{} _,Var j@Internal{} _] t)
  | (Apply "+" [a,b] _) <- lookupBlock i block
  , (Apply "+" [c,d] _) <- lookupBlock j block
  , a == c
  = Left $ Apply "-" [b,d] t
simplify' block (Apply "==" [x,Var i@Internal{} _] t)
  | (Apply "+" [y,c@Const{}] _) <- lookupBlock i block = Right $ do
      j <- simplify $ Apply "-" [x,y] (coerce (typeRef x) (typeRef y))
      simplify $ Apply "==" [j,c] t
  | (Apply "-" [y,c@Const{}] _) <- lookupBlock i block = Right $ do
      j <- simplify $ Apply "-" [y,x] (coerce (typeRef y) (typeRef x))
      simplify $ Apply "==" [j,c] t
simplify' block (Apply "==" [Var i@Internal{} _,x] t)
  | (Apply "+" [y,c@Const{}] _) <- lookupBlock i block = Right $ do
      j <- simplify $ Apply "-" [x,y] (coerce (typeRef x) (typeRef y))
      simplify $ Apply "==" [j,c] t
  | (Apply "-" [y,c@Const{}] _) <- lookupBlock i block = Right $ do
      j <- simplify $ Apply "-" [y,x] (coerce (typeRef y) (typeRef x))
      simplify $ Apply "==" [j,c] t
simplify' block (Apply "exp" [Var i@Internal{} _] _)
  | (Apply "log" [j] _) <- lookupBlock i block = Right $ return j
simplify' block (Apply "log" [Var i@Internal{} _] _)
  | (Apply "exp" [j] _) <- lookupBlock i block = Right $ return j
simplify' block (Apply "det" [Var i@Internal{} _] t)
  | (Apply "eye" _ _) <- lookupBlock i block = Right . return $ Const 1 t
simplify' block (Apply "not" [Var i@Internal{} _] _)
  | (Apply "not" [j] _) <- lookupBlock i block = Right $ return j
simplify' block (Apply "*" [Var i@Internal{} t,_] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right . return $ Var i t
simplify' block (Apply "*" [_,Var i@Internal{} t] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right . return $ Var i t
simplify' block (Apply "/" [Var i@Internal{} t,_] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right . return $ Var i t
simplify' block (Apply "+" [Var i@Internal{} _,a] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right $ return a
simplify' block (Apply "+" [a,Var i@Internal{} _] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right $ return a
simplify' block (Apply "-" [a,Var i@Internal{} _] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right $ return a
simplify' block (Apply "-" [Var i@Internal{} _,a] t)
  | Apply "zeros" _ _ <- lookupBlock i block = Right . simplify $ Apply "negate" [a] t
simplify' block (Apply "negate" [Var i@Internal{} t] _)
  | Apply "zeros" _ _ <- lookupBlock i block = Right . return $ Var i t
simplify' block (Apply "<>" [Var i@Internal{} _,a] t)
  | Apply "zeros" [r,_] _ <- lookupBlock i block = Right $ do
      c <- matrixCols' a
      simplify $ Apply "zeros" [r,c] t
simplify' block (Apply "<>" [a,Var i@Internal{} _] t)
  | Apply "zeros" [_,c] _ <- lookupBlock i block = Right $ do
      r <- matrixRows' a
      simplify $ Apply "zeros" [r,c] t
simplify' block (Apply "+s" js t) | js /= js' = Right . simplify $ Apply "+s" js' t
  where js' = do
          j <- js
          case j of
            Var i@Internal{} _ -> case lookupBlock i block of
              Apply "zeros" _ _ -> mzero
              _ -> return j
            Const c _ | isZeros c -> mzero
            _ -> return j
simplify' _ n = Left n

matrixRows' = matrixRowsCols False
matrixCols' = matrixRowsCols True
matrixRowsCols :: Bool -> NodeRef -> State Block NodeRef
matrixRowsCols cols m@(Var i@Internal{} (ArrayT (Just "matrix") _ _)) = do
  block <- get
  case lookupBlock i block of
    Apply op [a,b] _ | op `elem` ["+","-","*","/"] ->
      case typeRef a of
        ArrayT (Just "matrix") _ _ -> do
          n <- matrixRowsCols cols a
          if getConstVal n /= Just 1 then return n else case typeRef b of
            ArrayT (Just "matrix") _ _ -> matrixRowsCols cols b
            _ -> return n
        _ -> matrixRowsCols cols b
    Apply "<>" [a,b] _ -> matrixRowsCols cols $ if cols then b else a
    Apply "negate" [a] _ -> matrixRowsCols cols a
    Apply "asRow" _ _ | not cols -> return $ Const 1 IntT
    Apply "ifThenElse" [c,a,b] _ -> do
      a' <- matrixRowsCols cols a
      b' <- matrixRowsCols cols b
      simplify $ Apply "ifThenElse" [c,a',b'] IntT
    Apply "zeros" [r,c] _ -> return $ if cols then c else r
    Apply "eye" [n] _ -> return n
    Apply f (m:_) _ | f `elem` ["replaceIndex","deleteIndex","insertIndex"], cols ->
      matrixRowsCols cols m
    n -> error $ "matrixRowsCols "++ show n
matrixRowsCols cols m | ArrayT (Just "matrix") _ _ <- typeRef m =
  simplify $ Apply (if cols then "matrixCols" else "matrixRows") [m] IntT
matrixRowsCols _ r = error $ "matrixRowsCols "++ show r

simplifyDet :: Bool -> NodeRef -> Type -> Maybe (State Block NodeRef)
simplifyDet lg i t
  | isZ . sequence $ getConstVal <$> concat a =
    Just . return $ Const (if lg then log 0 else 0) t
  | isZ . sequence $ getConstVal <$> th, notNull tt, notNull `all` tt = Just $ do
      x <- det' [[hh]]
      y <- det' tt
      simplify $ Apply (if lg then "+" else "*") [x,y] t
  -- TODO: negate if matrix has even number of total rows
  -- TODO: avoid infinite loop when first column is zero
  -- | getConstVal hh == Just 0 = Just . det' $ tail a ++ [head a]
  | otherwise = Nothing
  where a = fromBlockMatrix i
        hh = head $ head a
        th = tail $ head a
        tt = tail <$> tail a
        isZ = maybe False $ all isZeros
        det x | isScalarD x = if lg then log x else x
              | lg        = AA.logDet x
              | otherwise = AA.det x
        det' = fromDExpr . det . DExpr . blockMatrix'

simplifyConj :: [NodeRef] -> State Block NodeRef
simplifyConj [r] = return r
simplifyConj [Const 1 _,r] = return r
simplifyConj refs = do
  block <- get
  let conj = simplifyConj' block refs
      nots = do
        ref <- conj
        case ref of
          Var i@Internal{} _ | Apply "not" j _ <- lookupBlock i block -> j
          _ -> mzero
      contra = ((Just 0 ==) . getConstVal) `any` conj ||
               notNull (conj `intersect` nots)
  if length conj == 1 then return $ head conj
  else if contra then fromDExpr false
  else if length conj == 2 then liftcons $ Apply "&&" conj boolT
  else liftcons $ Apply "&&s" conj boolT

simplifyConj' :: Block -> [NodeRef] -> [NodeRef]
simplifyConj' block refs = nub . sort $ do
  ref <- refs
  case ref of
    Const 1 _ -> mzero
    Var i@Internal{} _ | Apply f js _ <- lookupBlock i block
                       , f `elem` ["&&","&&s"] -> simplifyConj' block js
    _ -> return ref

apply :: String -> Type -> [Expr a] -> Expr r
apply f t = Expr . apply' f t . map erase
apply' :: String -> Type -> [DExpr] -> DExpr
apply' f t xs = DExpr $ do
    js <- mapM fromDExpr xs
    simplify $ Apply f js t

apply2 :: String -> Type -> Expr a -> Expr b -> Expr r
apply2 f t x y = Expr $ apply2' f t (erase x) (erase y)
apply2' :: String -> Type -> DExpr -> DExpr -> DExpr
apply2' f t x y = DExpr $ do
    i <- fromDExpr x
    j <- fromDExpr y
    simplify $ Apply f [i,j] t

applyClosed1 :: String -> Expr a -> Expr a
applyClosed1 f = Expr . applyClosed1' f . erase
applyClosed1' :: String -> DExpr -> DExpr
applyClosed1' f x = DExpr $ do
    i <- fromDExpr x
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
        x = DExpr . return $ Unconstrained IntT

makeArray :: Maybe String -> AA.AbstractArray DExpr DExpr
          -> [AA.Interval NodeRef] -> State Block NodeRef
makeArray l ar sh = do
    d <- getNextLevel
    let ids = [(Dummy d i, IntT) | i <- [1..length sh]]
        e = ar ! [DExpr . return $ Var i t | (i,t) <- ids]
    lam <- runLambda ids (fromDExpr e)
    t <- typeDExpr e
    hashcons $ Array sh lam (ArrayT l sh t)

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
    if varies dag (map fst sh ++ map snd sh) ||
       (not . null $ inputs dag `intersect` capture ar)
      then makeArray l ar sh
      else liftBlock $ floatArray l ar

-- TODO: reduce code duplication
floatNode :: Node -> State Block NodeRef
floatNode a = do
    block <- get
    let dag = topDAG block
        stick = case a of
          Array sh lam _ ->
            varies dag (map fst sh ++ map snd sh) || variesLambda dag lam
          FoldScan _ _ lam sd ls _ ->
            varies dag [sd,ls] || variesLambda dag lam
          Case hd alts _ ->
            varies dag [hd] || variesLambda' dag `any` alts
    if stick then hashcons a
    else liftBlock $ floatNode a

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
        s = typeIndex 1 $ typeRef l
        TypeIs t = typeOf :: TypeOf b
        i = expr . return $ Var (Dummy d 1) s
        j = expr . return $ Var (Dummy d 2) t
        -- TODO: runLambda
        (ret, Block (dag:block')) = runState (fromExpr $ f i j) $
          deriveBlock (DAG d [(Dummy d 1,s), (Dummy d 2,t)] Bimap.empty) block
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

-- find leftmost element of v satisfying p, else def if no elements satisfy p
find' :: (ScalarType e) => (Expr e -> B) -> Expr e -> Expr [e] -> Expr e
find' p def v = foldr f def v where f i j = ifB (p i) i j

findSortedInsertIndex :: R -> RVec -> Z
findSortedInsertIndex = apply2 "findSortedInsertIndex" IntT

min' = applyClosed2 "min"


------------------------------------------------------------------------------
-- INSTANCES                                                                --
------------------------------------------------------------------------------

instance Num DExpr where
    (+) = applyClosed2' "+"
    (-) = applyClosed2' "-"
    (*) = applyClosed2' "*"
    fromInteger x = --trace ("WARN assuming IntT type for DExpr "++ show x) $
      DExpr . return $ Const (fromInteger x) IntT
instance Fractional DExpr where
    (/) = applyClosed2' "/"
instance Floating DExpr where
    pi = apply' "pi" RealT []
    (**) = applyClosed2' "**"
    exp   = applyClosed1' "exp"
    log   = applyClosed1' "log"
    sqrt  = applyClosed1' "sqrt"
    sin   = applyClosed1' "sin"
    cos   = applyClosed1' "cos"
    tan   = applyClosed1' "tan"
    asin  = applyClosed1' "asin"
    acos  = applyClosed1' "acos"
    atan  = applyClosed1' "atan"
    sinh  = applyClosed1' "sinh"
    cosh  = applyClosed1' "cosh"
    tanh  = applyClosed1' "tanh"
    asinh = applyClosed1' "asinh"
    acosh = applyClosed1' "acosh"
    atanh = applyClosed1' "atanh"

factorial' :: Z -> Z
factorial' n = apply "factorial" IntT [n]
logFactorial' :: Z -> R
logFactorial' n = apply "logFactorial" RealT [n]

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

instance (ScalarType e) => AA.Vector (Expr [e]) Z (Expr e) where
    vector = array (Just "vector") 1
    blockVector = Expr . AA.blockVector . map erase
    vectorSize  = Expr . AA.vectorSize . erase
instance AA.Vector DExpr DExpr DExpr where
    vector = array' (Just "vector") 1
    blockVector v = DExpr $ do
      k <- sequence $ fromDExpr <$> v
      let bnd = ([1],[length v])
          ns = do
            t <- typeRef <$> k
            return $ case t of
              _ | isScalar t -> Const 1 IntT
              ArrayT _ [(Const 1 IntT,hi)] _ -> hi
      n <- simplify $ Apply "+s" ns IntT
      let kind = case [s | ArrayT (Just s) _ _ <- typeRef <$> k] of
                   [] -> Nothing
                   xs -> Just $ unreplicate xs
          t = ArrayT kind [(Const 1 IntT,n)] $
            coerces $ [t' | ArrayT _ _ t' <- typeRef <$> k] ++ filter isScalar (typeRef <$> k)
          k' = sequence $ getConstVal <$> k
      return $ if isZero `all` k then Const 0 t else
               if isJust k' then Const (fromList $ fromJust k') t else
        BlockArray (A.array bnd [([i], k !! (i-1)) | [i] <- A.range bnd]) t
      where isZero (Const c _) | c == 0 = True
            isZero _ = False
    vectorSize e | isScalarD e = 1
    vectorSize v = apply' "vectorSize" IntT [v]

blockMatrix' :: [[NodeRef]] -> State Block NodeRef
blockMatrix' [] = error "blockMatrix' []"
blockMatrix' rows | null `any` rows = error $ "blockMatrix' "++ show rows
blockMatrix' [[r]] = return r
blockMatrix' k' | not sane = trace ("WARN mis-shaped blocks, assuming incoherent: "++ show k) $
                  return $ Unconstrained UnknownType
                | otherwise = do
  r <- simplify $ Apply "+s" rs IntT
  c <- simplify $ Apply "+s" cs IntT
  let t = ArrayT (Just "matrix") [(Const 1 IntT,r),(Const 1 IntT,c)] $
        coerces [t' | ArrayT _ _ t' <- typeRef <$> concat k]
  return . flip BlockArray t $ A.array bnd
    [([i,j], k !! (i-1) !! (j-1)) | [i,j] <- A.range bnd]
  where flattenRow = foldr1 (zipWith (++))
        flattenMatrix = concat . map flattenRow :: [[[[a]]]] -> [[a]]
        k | isBlockArray `all` concat k' = flattenMatrix $ map fromBlockMatrix <$> k'
          | otherwise = k'
        bnd = ([1,1],[length k, length $ head k])
        rs = do
          t <- typeRef . head <$> k
          return $ case t of
            _ | isScalar t -> Const 1 IntT
            ArrayT _ [(Const 1 IntT,hi),_] _ -> hi
            _ -> error $ "blockMatrix'.rs "++ show t
        cs = do
          t <- typeRef <$> head k
          return $ case t of
            _ | isScalar t -> Const 1 IntT
            ArrayT _ [_,(Const 1 IntT,hi)] _ -> hi
            _ -> error $ "blockMatrix'.cs "++ show t
        sane = allSame (map length k) && saneRow `all` k && saneCol `all` transpose k
        saneRow row = allSame' $ do
          t <- typeRef <$> row
          return $ case t of
            _ | isScalar t -> Const 1 IntT
            ArrayT _ [(Const 1 IntT,hi),_] _ -> hi
            _ -> error $ "saneRow "++ show t
        saneCol col = allSame' $ do
          t <- typeRef <$> col
          return $ case t of
            _ | isScalar t -> Const 1 IntT
            ArrayT _ [_,(Const 1 IntT,hi)] _ -> hi
            _ -> error $ "saneCol "++ show t
        allSame' xs = allSame xs || isUnconstrained `any` xs

instance (ScalarType e) => AA.Matrix (Expr [[e]]) Z (Expr e) where
    matrix = array (Just "matrix") 2
instance AA.Matrix DExpr DExpr DExpr where
    matrix = array' (Just "matrix") 2
    blockMatrix m = DExpr $ do
      k' <- sequence $ sequence . map asMatrixD <$> m
      blockMatrix' k'
      where asMatrixD a = do
              t <- typeDExpr a
              fromDExpr $ case t of
                (ArrayT (Just "row_vector") [_] _) -> AA.asRow a
                (ArrayT _ [_] _) -> AA.asColumn a
                (ArrayT _ [_,_] _) -> a
                _ | isScalar t -> a
    eye n = DExpr $ do
      n' <- fromDExpr n
      let t = ArrayT (Just "matrix") [(Const 1 IntT,Unconstrained IntT)
                                     ,(Const 1 IntT,Unconstrained IntT)] RealT
      simplify $ Apply "eye" [n'] t
    zeros m n = DExpr $ do
      m' <- fromDExpr m
      n' <- fromDExpr n
      let t = ArrayT (Just "matrix") [(Const 1 IntT,Unconstrained IntT)
                                     ,(Const 1 IntT,Unconstrained IntT)] RealT
      simplify $ Apply "zeros" [m',n'] t

typeMatrixProduct (ArrayT _ [r,_] t) (ArrayT _ [_,c] _) = ArrayT (Just "matrix") [r,c] t

instance (ScalarType e) => Monoid (Expr [[e]]) where
    mappend a b = Expr $ erase a `mappend` erase b
instance Monoid DExpr where
    mappend a b = DExpr $ do
        i <- fromDExpr a
        j <- fromDExpr b
        simplify . Apply "<>" [i,j] $ typeRef i `typeMatrixProduct` typeRef j

instance AA.Indexable (Expr [e]) (Expr Integer) (Expr e) where
    a ! e = Expr $ erase a ! erase e
    deleteIndex  a e   = Expr $ AA.deleteIndex  (erase a) (erase e)
    insertIndex  a e d = Expr $ AA.insertIndex  (erase a) (erase e) (erase d)
    replaceIndex a e d = Expr $ AA.replaceIndex (erase a) (erase e) (erase d)
instance AA.Indexable DExpr DExpr DExpr where
    a ! e = DExpr $ do
        f <- fromDExpr a
        j <- fromDExpr e
        return $ case f of
            (Const v (ArrayT _ [_] t)) | (Const i _) <- j -> Const (v![integer i]) t
            (Index g js) -> Index g (j:js)
            _            -> Index f [j]
    deleteIndex a e = DExpr $ do
      f <- fromDExpr a
      j <- fromDExpr e
      let (ArrayT nm ((lo,hi):sh) t) = typeRef f
      hi' <- simplify $ Apply "-" [hi, Const 1 IntT] (typeRef hi)
      let t' = ArrayT nm ((lo,hi'):sh) t
      simplify $ Apply "deleteIndex" [f,j] t'
    insertIndex a e d = DExpr $ do
      f <- fromDExpr a
      i <- fromDExpr e
      j <- fromDExpr d
      let (ArrayT nm ((lo,hi):sh) t) = typeRef f
      hi' <- simplify $ Apply "+" [hi, Const 1 IntT] (typeRef hi)
      let t' = ArrayT nm ((lo,hi'):sh) t
      simplify $ Apply "insertIndex" [f,i,j] t'
    replaceIndex a e d = DExpr $ do
      f <- fromDExpr a
      i <- fromDExpr e
      j <- fromDExpr d
      simplify $ Apply "replaceIndex" [f,i,j] (typeRef f)

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

instance LA.Transposable DExpr DExpr where
    tr m = DExpr $ do
        i <- fromDExpr m
        let (ArrayT _ [r,c] t) = typeRef i
        simplify $ Apply "tr" [i] (ArrayT (Just "matrix") [c,r] t)
    tr' m = DExpr $ do
        i <- fromDExpr m
        let (ArrayT _ [r,c] t) = typeRef i
        simplify $ Apply "tr'" [i] (ArrayT (Just "matrix") [c,r] t)
instance (ScalarType e) => LA.Transposable (Expr [[e]]) (Expr [[e]]) where
    tr  = Expr . LA.tr  . erase
    tr' = Expr . LA.tr' . erase

instance AA.InnerProduct (Expr [e]) (Expr e) where
    u <.> v = expr $ do
        i <- fromExpr u
        j <- fromExpr v
        let (ArrayT _ _ t) = typeRef i
        simplify $ Apply "<.>" [i,j] t

instance AA.LinearOperator DExpr DExpr where
    m #> v = DExpr $ do
        i <- fromDExpr m
        j <- fromDExpr v
        let (ArrayT _ [r,_] t) = typeRef i
        simplify $ Apply "#>" [i,j] (ArrayT (Just "vector") [r] t)
    v <# m = DExpr $ do
        i <- fromDExpr v
        j <- fromDExpr m
        let (ArrayT _ [_,c] t) = typeRef j
        simplify $ Apply "<#" [i,j] (ArrayT (Just "row_vector") [c] t)
    diag v = DExpr $ do
        i <- fromDExpr v
        let (ArrayT _ [n] t) = typeRef i
        simplify $ Apply "diag" [i] (ArrayT (Just "matrix") [n,n] t)
    asColumn v = DExpr $ do
        k <- fromDExpr v
        let (ArrayT _ [n] t) = typeRef k
            t' = ArrayT (Just "matrix") [n,(Const 1 IntT, Const 1 IntT)] t
        case k of
          BlockArray a _ -> do
            let [(1,n')] = AA.shape a
            a' <- sequence $ A.array ([1,1],[n',1]) [([i,1], asCol' $ a![i]) | i <- [1..n']]
            return (BlockArray a' t')
          _ -> simplify $ Apply "asColumn" [k] t'
      where asCol' x | isScalar (typeRef x) = return x
                     | otherwise = fromDExpr . AA.asColumn . DExpr $ return x
    asRow v = DExpr $ do
        k <- fromDExpr v
        let (ArrayT _ [n] t) = typeRef k
            t' = ArrayT (Just "matrix") [(Const 1 IntT, Const 1 IntT),n] t
        case k of
          BlockArray a _ -> do
            let [(1,n')] = AA.shape a
            a' <- sequence $ A.array ([1,1],[1,n']) [([1,j], asRow' $ a![j]) | j <- [1..n']]
            return (BlockArray a' t')
          _ -> simplify $ Apply "asRow" [k] t'
      where asRow' x | isScalar (typeRef x) = return x
                     | otherwise = fromDExpr . AA.asRow . DExpr $ return x
instance AA.LinearOperator (Expr [[e]]) (Expr [e]) where
    m #> v = Expr $ erase m AA.#> erase v
    v <# m = Expr $ erase v AA.<# erase m
    diag     = Expr . AA.diag     . erase
    asColumn = Expr . AA.asColumn . erase
    asRow    = Expr . AA.asRow    . erase

instance AA.SquareMatrix DExpr DExpr where
    chol m = DExpr $ do
        i <- fromDExpr m
        let (ArrayT _ sh t) = typeRef i
        simplify $ Apply "chol" [i] (ArrayT (Just "matrix") sh t)
    inv m = DExpr $ do
        i <- fromDExpr m
        let (ArrayT _ sh t) = typeRef i
        simplify $ Apply "inv" [i] (ArrayT (Just "matrix") sh t)
    det m = DExpr $ do
        i <- fromDExpr m
        let (ArrayT _ _ t) = typeRef i
        simplify $ Apply "det" [i] t
    logDet m = DExpr $ do
        i <- fromDExpr m
        case i of
          Unconstrained{} -> return $ Unconstrained UnknownType
          _ -> let (ArrayT _ _ t) = trace ("logDet "++ show i) $ typeRef i
               in simplify $ Apply "log_det" [i] t
instance AA.SquareMatrix (Expr [[e]]) (Expr e) where
    chol   = Expr . AA.chol   . erase
    inv    = Expr . AA.inv    . erase
    det    = Expr . AA.det    . erase
    logDet = Expr . AA.logDet . erase

qfDiag :: RMat -> RVec -> RMat
qfDiag m v = expr $ do
  i <- fromExpr m
  j <- fromExpr v
  let (ArrayT _ sh t) = typeRef i
  simplify $ Apply "quad_form_diag" [i,j] (ArrayT Nothing sh t)

instance Boolean DExpr where
    true  = apply' "true" boolT []
    false = apply' "false" boolT []
    notB  = applyClosed1' "not"
    (&&*) = applyClosed2' "&&"
    (||*) = applyClosed2' "||"
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

detuple :: (ExprTuple t) => t -> Expr t
detuple t = expr $ do
  js <- sequence $ fromDExpr <$> fromExprTuple t
  return $ case js of
    [j] -> j
    _ -> Data 0 js . TupleT $ typeRef <$> js

entuple :: forall t. (ExprTuple t) => Expr t -> t
entuple = toExprTuple . entupleD n . erase
  where TupleSize n = tupleSize :: TupleSize t
entupleD :: Int -> DExpr -> [DExpr]
entupleD 1 e = [e]
entupleD n e = extractD e 0 <$> [0..(n-1)]

newtype TupleSize t = TupleSize Int
class ExprTuple t where
    tupleSize :: TupleSize t
    fromExprTuple :: t -> [DExpr]
    toExprTuple :: [DExpr] -> t
    fromConstVals :: [ConstVal] -> t

zipExprTuple :: (ExprTuple t) => t -> t -> [(DExpr,DExpr)]
zipExprTuple s t = fromExprTuple s `zip` fromExprTuple t

constDExpr :: ConstVal -> Type -> DExpr
constDExpr c = DExpr . return . Const c

const :: (ScalarType t) => ConstVal -> Expr t
const = constExpr

instance (ScalarType a) => ExprTuple (Expr a) where
    tupleSize = TupleSize 1
    fromExprTuple (a) = [erase a]
    toExprTuple [a] = (Expr a)
    fromConstVals [a] = (const a)
instance (ScalarType a, ScalarType b) =>
         ExprTuple (Expr a, Expr b) where
    tupleSize = TupleSize 2
    fromExprTuple (a,b) = [erase a, erase b]
    toExprTuple [a,b] = (Expr a, Expr b)
    fromConstVals [a,b] = (const a, const b)
instance (ScalarType a, ScalarType b, ScalarType c) =>
         ExprTuple (Expr a, Expr b, Expr c) where
    tupleSize = TupleSize 3
    fromExprTuple (a,b,c) = [erase a, erase b, erase c]
    toExprTuple [a,b,c] = (Expr a, Expr b, Expr c)
    fromConstVals [a,b,c] = (const a, const b, const c)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d) where
    tupleSize = TupleSize 4
    fromExprTuple (a,b,c,d) = [erase a, erase b, erase c, erase d]
    toExprTuple [a,b,c,d] = (Expr a, Expr b, Expr c, Expr d)
    fromConstVals [a,b,c,d] = (const a, const b, const c, const d)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e) where
    tupleSize = TupleSize 5
    fromExprTuple (a,b,c,d,e) =
      [erase a, erase b, erase c, erase d, erase e]
    toExprTuple [a,b,c,d,e] =
      (Expr a, Expr b, Expr c, Expr d, Expr e)
    fromConstVals [a,b,c,d,e] =
      (const a, const b, const c, const d, const e)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f) where
    tupleSize = TupleSize 6
    fromExprTuple (a,b,c,d,e,f) =
      [erase a, erase b, erase c, erase d, erase e, erase f]
    toExprTuple [a,b,c,d,e,f] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f)
    fromConstVals [a,b,c,d,e,f] =
      (const a, const b, const c, const d, const e, const f)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f, ScalarType g) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g) where
    tupleSize = TupleSize 7
    fromExprTuple (a,b,c,d,e,f,g) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g]
    toExprTuple [a,b,c,d,e,f,g] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g)
    fromConstVals [a,b,c,d,e,f,g] =
      (const a, const b, const c, const d, const e, const f, const g)
instance (ScalarType a, ScalarType b, ScalarType c, ScalarType d,
          ScalarType e, ScalarType f, ScalarType g, ScalarType h) =>
         ExprTuple (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g, Expr h) where
    tupleSize = TupleSize 8
    fromExprTuple (a,b,c,d,e,f,g,h) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g, erase h]
    toExprTuple [a,b,c,d,e,f,g,h] =
      (Expr a, Expr b, Expr c, Expr d, Expr e, Expr f, Expr g, Expr h)
    fromConstVals [a,b,c,d,e,f,g,h] =
      (const a, const b, const c, const d, const e, const f, const g, const h)

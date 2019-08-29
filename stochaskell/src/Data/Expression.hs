{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables,
  TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts, ConstraintKinds, RankNTypes, TypeOperators,
  DefaultSignatures, UndecidableInstances #-}
module Data.Expression where

import qualified Data.Array as A
import qualified Data.Array.Abstract as AA
import Data.Array.Abstract ((!))
import qualified Data.Bimap as Bimap
import Data.Boolean
import Data.Char
import Data.Expression.Const hiding (isScalar)
import Data.List
import Data.List.Extra
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
import GHC.Generics hiding (Constructor,R,P)
import qualified Numeric.LinearAlgebra as LA
import Util


------------------------------------------------------------------------------
-- EXPRESSIONS                                                              --
------------------------------------------------------------------------------

type Pointer = Int
type Level = Int
type NS = String

-- | variable identifier
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

-- | reference to a node in the computation graph
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

-- | create a symbolic variable
symbol :: forall t. (ExprType t) => String -> Expression t
symbol name = expr . return $ Var (Symbol name False) t
  where TypeIs t = typeOf :: TypeOf t
-- | create a 'symbol' assumed to have a known value
constSymbol :: forall t. (ExprType t) => String -> Expression t
constSymbol name = expr . return $ Var (Symbol name True) t
  where TypeIs t = typeOf :: TypeOf t
-- | create a list of (one-letter) 'symbol's
symbols :: (ExprType t) => String -> [Expression t]
symbols names = symbol . return <$> names

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
          | Unfold
                  { uFunc :: Lambda NodeRef
                  , uSeed :: NodeRef
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
    | otherwise = f ++" "++ unwords (map show args) ++" :: "++ show t
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
                    | otherwise = "C"++ show i ++" "++ unwords (map show $ inputs dag)
                rhs = indent (showLet dag ret)
            return $ lhs ++" ->\n"++ rhs
  show (Unfold (Lambda dag hd) seed _) =
    "unfold "++ show seed ++" $ \\"++ show (head $ inputs' dag) ++" ->\n"++
      indent (showLet dag hd)
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
inputsLevel :: Level -> DAG -> [(Id,Type)]
inputsLevel d body = do
  (i,t) <- inputsT body
  let i' = case i of
        -- TODO rename to avoid capture without breaking fixpt
        Dummy _ p -> Dummy d p -- set correct level
  return (i',t)
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
showBlock block r = foldl (flip showLet') r block
showBlock' :: Block -> String -> String
showBlock' (Block ds) = showBlock ds

-- | dynamically typed Stochaskell expression
newtype DExpr = DExpr { fromDExpr :: State Block NodeRef }
runDExpr :: DExpr -> (NodeRef, Block)
runDExpr e = runState (fromDExpr e) emptyBlock
instance Eq DExpr where
  e == f = runDExpr e == runDExpr f
instance Ord DExpr where
  e `compare` f = runDExpr e `compare` runDExpr f
instance Show DExpr where
  show e = showBlock block $ show ret
    where (ret, Block block) = runDExpr e

data LVal = LVar Id
          | LSub Id [DExpr]
          | LField Id Type Tag Int
          | LCond Bool LVal DExpr
          | LConstr DExpr
          | LUnfold (Lambda NodeRef)
          deriving (Eq, Ord, Show)

getId' :: LVal -> Maybe Id
getId' (LVar i ) = Just i
getId' (LSub i _) = Just i
getId' (LField i _ _ _) = Just i
getId' (LCond _ l _) = getId' l
getId' LConstr{} = Nothing

newtype EEnv = EEnv (Map LVal DExpr)
emptyEEnv :: EEnv
emptyEEnv = EEnv Map.empty

instance Show EEnv where
  show (EEnv m) = unlines $
    [show i ++" :=\n"++ indent (show v) | (LVar i, v) <- Map.toAscList m] ++
    ["{"++ show c ++"} "++ show l ++" :=\n"++ indent (show v) | (LCond _ l c, v) <- Map.toAscList m]

unionEEnv :: EEnv -> EEnv -> EEnv
unionEEnv (EEnv a) (EEnv b) = EEnv $ Map.union a b
unionsEEnv :: [EEnv] -> EEnv
unionsEEnv envs = EEnv $ Map.unions [env | EEnv env <- envs]

lookupEEnv :: Id -> EEnv -> Maybe DExpr
lookupEEnv k (EEnv m) = Map.lookup (LVar k) m
insertEEnv :: Id -> DExpr -> EEnv -> EEnv
insertEEnv k v (EEnv m) = EEnv $ Map.insert (LVar k) v m
filterEEnv :: (LVal -> DExpr -> Bool) -> EEnv -> EEnv
filterEEnv p (EEnv m) = EEnv $ Map.filterWithKey p m

conditionEEnv :: Bool -> DExpr -> EEnv -> EEnv
conditionEEnv sufficient c (EEnv env) =
  EEnv $ Map.mapKeys (\k -> LCond sufficient k c) env

bindInputs :: DAG -> [DExpr] -> EEnv
bindInputs dag xs = EEnv . Map.fromList $ inputsL dag `zip` xs

-- | Stochaskell expression representation (statically typed)
newtype Expression t = Expression { erase :: DExpr }
type Expr t = Expression t
expr :: State Block NodeRef -> Expression t
expr = Expression . DExpr
fromExpr :: Expression t -> State Block NodeRef
fromExpr = fromDExpr . erase
runExpr :: Expression t -> (NodeRef, Block)
runExpr  =  runDExpr . erase
instance (Eq t) => Eq (Expression t) where
  e == f = runExpr e == runExpr f
instance (Ord t) => Ord (Expression t) where
  e `compare` f = runExpr e `compare` runExpr f

runExprs :: (ExprTuple t) => t -> ([NodeRef], Block)
runExprs = flip runState emptyBlock . mapM fromDExpr . fromExprTuple

instance (ExprTuple a, ExprType t) => Show (a -> Expression t) where
  show f = "\\input ->\n"++ (indent . show . erase $ f (entuple $ symbol "input"))
instance (ExprTuple a, ExprTuple b, ExprType t) => Show (a -> b -> Expression t) where
  show f = "\\input_a input_b ->\n"++ (indent . show . erase $
    f (entuple $ symbol "input_a") (entuple $ symbol "input_b"))

type B = Expression Bool           -- ^ boolean
type R = Expression Double         -- ^ real
type Z = Expression Integer        -- ^ integer
type BVec = Expression [Bool]      -- ^ boolean vector
type RVec = Expression [Double]    -- ^ real vector
type ZVec = Expression [Integer]   -- ^ integer vector
type BMat = Expression [[Bool]]    -- ^ boolean matrix
type RMat = Expression [[Double]]  -- ^ real matrix
type ZMat = Expression [[Integer]] -- ^ integer matrix


------------------------------------------------------------------------------
-- TYPES                                                                    --
------------------------------------------------------------------------------

-- TODO: remove NodeRef's from Type, make subranges static and
--       query array sizes elsewhere
-- | Stochaskell types
data Type
    = IntT
    | RealT
    | SubrangeT Type (Maybe NodeRef) (Maybe NodeRef)
    -- TODO: better encoding of constraints than (Maybe String)
    | ArrayT (Maybe String) [AA.Interval NodeRef] Type
    | TupleT [Type]
    | UnionT [[Type]]
    | RecursiveT
    | UnknownType
    deriving (Eq, Ord)
boolT :: Type
boolT = SubrangeT IntT (Just $ Const 0 IntT) (Just $ Const 1 IntT)

tupleT :: [Type] -> Type
tupleT [t] = t
tupleT ts = TupleT ts

vecT :: Type -> Type
vecT t | isScalar t = ArrayT (Just "vector") [(Const 1 IntT, Unconstrained IntT)] t
matT :: Type -> Type
matT t | isScalar t = ArrayT (Just "matrix") [(Const 1 IntT, Unconstrained IntT)
                                             ,(Const 1 IntT, Unconstrained IntT)] t

instance Show Type where
  show IntT = "Z"
  show RealT = "R"
  show (SubrangeT IntT (Just (Const 0 IntT)) (Just (Const 1 IntT))) = "B"
  show (SubrangeT t a b) = unwords ["Subrange", show t, show a, show b]
  show (ArrayT Nothing sh t) = unwords ["Array", show sh, show t]
  --show (ArrayT (Just name) sh t) = unwords [name, show sh, show t]
  show (ArrayT (Just "vector") _ t) | isScalar t = show t ++"Vec"
  show (ArrayT (Just "matrix") _ t) | isScalar t = show t ++"Mat"
  show (ArrayT (Just name) _ t) = name ++" "++ show t
  show (TupleT ts) = show ts
  show (UnionT ts) = "Union"++ show ts
  show RecursiveT = "MU"
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

newtype TypeOf t = TypeIs Type deriving (Show)
-- | Stochaskell interface for native Haskell types
class ExprType t where
    -- | corresponding Stochaskell type
    typeOf       :: TypeOf t
    default typeOf :: (Generic t, GConstructor (Rep t)) => TypeOf t
    typeOf = TypeIs t
      where TypeIs t = gtypeOf :: TypeOf (Rep t p)
    -- | convert constant to native value
    toConcrete   :: ConstVal -> t
    default toConcrete :: (Constructor t) => ConstVal -> t
    toConcrete   = toConcreteC
    -- | convert native value to Stochaskell expression
    fromConcrete :: t -> Expression t
    default fromConcrete :: (Constructor t) => t -> Expression t
    fromConcrete = fromConcreteC
    -- | convert native value to constant
    constVal     :: t -> ConstVal
    default constVal :: (ToConstVal t) => t -> ConstVal
    constVal     = toConstVal
    -- | convert constant to Stochaskell expression
    constExpr    :: ConstVal -> Expression t
    constExpr    = fromConcrete . toConcrete
instance ExprType Integer where
    typeOf       = TypeIs IntT
    toConcrete   = toInteger
    fromConcrete = fromInteger
    constVal     = fromInteger
    constExpr c  = expr . return $ Const c IntT
instance ExprType Bool where
    typeOf       = TypeIs boolT
    toConcrete   = toBool
    fromConcrete b = if b then true else false
    constVal     b = if b then true else false
    constExpr c  = expr . return $ Const c boolT
instance ExprType Double where
    typeOf       = TypeIs RealT
    toConcrete   = toDouble
    fromConcrete = fromRational . toRational
    constVal     = fromRational . toRational
    constExpr c  = expr . return $ Const c RealT
instance forall t. (ExprType t) => ExprType [t] where
    typeOf = TypeIs $ case t of
      _ | isScalar t -> vecT t
      ArrayT (Just "vector") [(Const 1 IntT, Unconstrained IntT)] t -> matT t
      where TypeIs t = typeOf :: TypeOf t
    toConcrete   = map toConcrete . toList
    fromConcrete = constExpr . constVal
    constVal     = fromList . map constVal
    constExpr c  = expr . return . Const c $ ArrayT Nothing sh t
      where (lo,hi) = AA.bounds c
            f = flip Const IntT . fromInteger
            sh = map f lo `zip` map f hi
            t = case typeOf :: TypeOf t of
              TypeIs s | isScalar s -> s
              TypeIs (ArrayT _ _ s) -> s

instance forall a. (ExprType a) => ExprType (Expression a) where
  typeOf = TypeIs a where TypeIs a = typeOf :: TypeOf a
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b. (ExprType a, ExprType b) => ExprType (Expression a, Expression b) where
  typeOf = TypeIs $ TupleT [a,b]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c.
    (ExprType a, ExprType b, ExprType c) =>
    ExprType (Expression a, Expression b, Expression c) where
  typeOf = TypeIs $ TupleT [a,b,c]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d.
    (ExprType a, ExprType b, ExprType c, ExprType d) =>
    ExprType (Expression a, Expression b, Expression c, Expression d) where
  typeOf = TypeIs $ TupleT [a,b,c,d]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d e.
    (ExprType a, ExprType b, ExprType c, ExprType d,
     ExprType e) =>
    ExprType (Expression a, Expression b, Expression c, Expression d, Expression e) where
  typeOf = TypeIs $ TupleT [a,b,c,d,e]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
          TypeIs e = typeOf :: TypeOf e
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d e f.
    (ExprType a, ExprType b, ExprType c, ExprType d,
     ExprType e, ExprType f) =>
    ExprType (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f) where
  typeOf = TypeIs $ TupleT [a,b,c,d,e,f]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
          TypeIs e = typeOf :: TypeOf e
          TypeIs f = typeOf :: TypeOf f
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d e f g.
    (ExprType a, ExprType b, ExprType c, ExprType d,
     ExprType e, ExprType f, ExprType g) =>
    ExprType (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g) where
  typeOf = TypeIs $ TupleT [a,b,c,d,e,f,g]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
          TypeIs e = typeOf :: TypeOf e
          TypeIs f = typeOf :: TypeOf f
          TypeIs g = typeOf :: TypeOf g
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d e f g h.
    (ExprType a, ExprType b, ExprType c, ExprType d,
     ExprType e, ExprType f, ExprType g, ExprType h) =>
    ExprType (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h) where
  typeOf = TypeIs $ TupleT [a,b,c,d,e,f,g,h]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
          TypeIs e = typeOf :: TypeOf e
          TypeIs f = typeOf :: TypeOf f
          TypeIs g = typeOf :: TypeOf g
          TypeIs h = typeOf :: TypeOf h
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined
instance forall a b c d e f g h i.
    (ExprType a, ExprType b, ExprType c, ExprType d,
     ExprType e, ExprType f, ExprType g, ExprType h, ExprType i) =>
    ExprType (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h, Expression i) where
  typeOf = TypeIs $ TupleT [a,b,c,d,e,f,g,h,i]
    where TypeIs a = typeOf :: TypeOf a
          TypeIs b = typeOf :: TypeOf b
          TypeIs c = typeOf :: TypeOf c
          TypeIs d = typeOf :: TypeOf d
          TypeIs e = typeOf :: TypeOf e
          TypeIs f = typeOf :: TypeOf f
          TypeIs g = typeOf :: TypeOf g
          TypeIs h = typeOf :: TypeOf h
          TypeIs i = typeOf :: TypeOf i
  toConcrete = undefined
  fromConcrete = undefined
  constVal = undefined

newtype Tags t = Tags [Tag]
-- | Stochaskell interface for algebraic data types
class ExprType c => Constructor c where
  -- | list of possible tags in tagged union
  tags :: Tags c
  default tags :: (Generic c, GConstructor (Rep c)) => Tags c
  tags = Tags t
    where Tags t = gtags :: Tags (Rep c p)
  -- | construct ADT from tag and arguments
  construct   :: (forall t. ExprType t => a -> Expression t) -> Tag -> [a] -> c
  default construct :: (Generic c, GConstructor (Rep c)) =>
                 (forall t. ExprType t => a -> Expression t) -> Tag -> [a] -> c
  construct f i xs = to (gconstruct f i xs)
  -- | deconstruct ADT to tag and arguments
  deconstruct :: (forall t. ExprType t => Expression t -> a) -> c -> (Tag, [a])
  default deconstruct :: (Generic c, GConstructor (Rep c)) =>
                 (forall t. ExprType t => Expression t -> a) -> c -> (Tag, [a])
  deconstruct f x = gdeconstruct f (from x)
  -- | convert constant to ADT value
  toConcreteC :: ConstVal -> c
  toConcreteC (Tagged c args) = construct constExpr c args
  -- | convert ADT value to Stochaskell expression
  fromConcreteC :: c -> Expression c
  fromConcreteC m = expr $ do
    js <- sequence args
    let TypeIs t = typeOf :: TypeOf c
    return $ Data c js t
    where (c, args) = deconstruct fromExpr m

newtype FixE f = FixE { unfixE :: FixE' f }
type FixE' f = Expression (f (FixE f))

class GConstructor f where
  gtypeOf :: TypeOf (f p)
  gtags :: Tags (f p)
  gtags = Tags [0]
  gconstruct   :: (forall t. ExprType t => a -> Expression t) -> Tag -> [a] -> f p
  gdeconstruct :: (forall t. ExprType t => Expression t -> a) -> f p -> (Tag, [a])
instance GConstructor U1 where
  gtypeOf = TypeIs (UnionT [[]])
  gconstruct f 0 [] = U1
  gdeconstruct f U1 = (0, [])
instance forall a b. (GConstructor a, GConstructor b) => GConstructor (a :*: b) where
  gtypeOf = TypeIs (UnionT [s ++ t])
    where TypeIs (UnionT [s]) = gtypeOf :: TypeOf (a p)
          TypeIs (UnionT [t]) = gtypeOf :: TypeOf (b p)
  gconstruct f 0 zs = a :*: b
    where TypeIs (UnionT [s]) = gtypeOf :: TypeOf (a p)
          n = length s
          xs = take n zs
          ys = drop n zs
          a = gconstruct f 0 xs
          b = gconstruct f 0 ys
  gdeconstruct f (a :*: b) = (0, xs ++ ys)
    where xs = snd (gdeconstruct f a)
          ys = snd (gdeconstruct f b)
instance forall a b. (GConstructor a, GConstructor b) => GConstructor (a :+: b) where
  gtypeOf = TypeIs (UnionT (s ++ t))
    where TypeIs (UnionT s) = gtypeOf :: TypeOf (a p)
          TypeIs (UnionT t) = gtypeOf :: TypeOf (b p)
  gtags = Tags [0..n-1]
    where Tags s = gtags :: Tags (a p)
          Tags t = gtags :: Tags (b p)
          n = length s + length t
  gconstruct f i xs = if i < offset then L1 (gconstruct f i xs)
                                    else R1 (gconstruct f (i - offset) xs)
    where Tags s = gtags :: Tags (a p)
          offset = length s
  gdeconstruct f (L1 x) = gdeconstruct f x
  gdeconstruct f (R1 x) = (i + offset, xs)
    where Tags s = gtags :: Tags (a p)
          offset = length s
          (i, xs) = gdeconstruct f x
instance forall i c a. (GConstructor a) => GConstructor (M1 i c a) where
  gtypeOf = TypeIs t
    where TypeIs t = gtypeOf :: TypeOf (a p)
  gtags = Tags t
    where Tags t = gtags :: Tags (a p)
  gconstruct f i xs = M1 (gconstruct f i xs)
  gdeconstruct f (M1 x) = gdeconstruct f x
instance (ExprType t) => GConstructor (K1 i (Expression t)) where
  gtypeOf = TypeIs (UnionT [[t]])
    where TypeIs t = typeOf :: TypeOf t
  gconstruct f 0 [x] = K1 (f x)
  gdeconstruct f (K1 x) = (0, [f x])
instance (ExprType (f (FixE f))) => GConstructor (K1 i (FixE f)) where
  gtypeOf = TypeIs (UnionT [[RecursiveT]])
  gconstruct f 0 [x] = K1 (FixE (f x))
  gdeconstruct f (K1 (FixE x)) = (0, [f x])

internal :: Level -> Pointer -> State Block NodeRef
internal level i =
    Var (Internal level i) <$> getType
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

typeExpr :: Expression t -> State Block Type
typeExpr = typeDExpr . erase

typeDExpr :: DExpr -> State Block Type
typeDExpr e = do
  i <- fromDExpr e
  return $ typeRef i

typeExprTuple :: (ExprTuple t) => t -> State Block [Type]
typeExprTuple = traverse typeDExpr . fromExprTuple

typeRef :: NodeRef -> Type
typeRef (Var _ t) = t
typeRef (Const _ t) = t
typeRef (Data _ _ t) = t
typeRef (BlockArray a t) = t
typeRef (Index a js) = case typeRef a of
  (ArrayT k sh t) | length js == length sh -> t
                  | otherwise -> ArrayT k (drop (length js) sh) t
  t -> trace ("ERROR cannot index non-array type "++ show t) UnknownType
typeRef (Extract v c k) | UnionT ts <- typeRef v = ts!!c!!k
typeRef (Extract v 0 k) | TupleT ts <- typeRef v = ts!!k
typeRef (Extract v c k) | UnknownType <- typeRef v = UnknownType
typeRef i@Extract{} = error $ "cannot extract invalid tag "++ show i
typeRef (Cond _ t) = t
typeRef (Unconstrained t) = t
typeRef (PartiallyConstrained _ _ _ t) = t

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
typeIndex 1 t = trace ("ERROR cannot index objects of type "++ show t) UnknownType
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
coerce r@(ArrayT (Just "row_vector") _ _) (ArrayT (Just "vector") sh t) =
  coerce r (ArrayT (Just "row_vector") sh t)
coerce v@(ArrayT (Just "vector") _ _) r@(ArrayT (Just "row_vector") _ _) = coerce r v
coerce s t = error $ "cannot coerce "++ show s ++" with "++ show t

coerces :: [Type] -> Type
coerces = foldr1 coerce

compatible :: Type -> Type -> Bool
compatible s t | s == t = True
compatible (SubrangeT s _ _) t = compatible s t
compatible s (SubrangeT t _ _) = compatible s t
compatible (ArrayT _ sh s) (ArrayT _ sh' t) | length sh == length sh' = compatible s t
compatible _ _ = False

-- | cast a value of one type to another
class    Cast a b                          where cast :: a -> b
instance Cast Z R                          where cast = expr . fromExpr
instance Cast Int R                        where cast = fromIntegral
instance Cast (Expression t) (Expression [t])          where cast = expr . fromExpr
instance Cast (Expression t) (Expression [[t]])        where cast = expr . fromExpr
instance (ExprType t) => Cast t (Expression t) where cast = fromConcrete


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
    block <- get
    let Block (dag:parent) = block
        (ref, parent') = runState s $ Block parent
    put $ deriveBlock dag parent'
    return ref

runLambda :: [(Id,Type)] -> State Block r -> State Block (Lambda r)
runLambda ids s = do
  block <- get
  let d = nextLevel block
      (ret, Block (dag:block')) = runState s $
          deriveBlock (DAG d ids Bimap.empty) block
  put $ Block block'
  if ((d /=) . idLevel . fst) `any` ids
  then error $ "runLambda "++ show ids ++" level "++ show d
  else return (Lambda dag ret)

-- does a list of expressions depend on the inputs to this block?
varies :: DAG -> [NodeRef] -> Bool
varies (DAG level inp _) xs = level == 0 || any p xs
  where p (Var i _) = i `elem` map fst inp || idLevel i == level
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
  (not . null $ inputs dag `intersect` collectIds adag) ||
  any p (collectIds adag)
  where p (Internal l _) = l == dagLevel dag
        p _ = False

collectIds :: DAG -> [Id]
collectIds dag = concatMap (f . snd) (nodes dag) \\ inputs dag
  where f (Apply _ args _) = concatMap g args
        f (Array sh (Lambda dag' r) _) =
          concatMap g (r : map fst sh ++ map snd sh) ++ collectIds dag'
        f (FoldScan _ _ (Lambda dag' r) sd ls _) =
          concatMap g [r,sd,ls] ++ collectIds dag'
        f (Case hd alts _) =
          concatMap g [hd] ++ concat [concatMap g rs ++ collectIds dag'
                                     | Lambda dag' rs <- alts]
        g (Var i _) = [i]
        g Const{} = []
        g (Data _ refs _) = concatMap g refs
        g (BlockArray a _) = concatMap g $ A.elems a
        g (Index a i) = g a ++ concatMap g i
        g (Extract d _ _) = g d
        g (Cond cvs _) = concatMap (g . fst) cvs ++ concatMap (g . snd) cvs
        g Unconstrained{} = []
        g PartiallyConstrained{} = []

-- recursive data dependencies
dependsNodeRef :: Block -> NodeRef -> Set Id
dependsNodeRef block ref = dependsType block (typeRef ref) `Set.union` go ref where
  go (Var i@Internal{} _) =
    Set.insert i . dependsNode block $ lookupBlock i block
  go (Var i _) = Set.singleton i
  go (Const _ _) = Set.empty
  go (BlockArray a _) = Set.unions $ go <$> A.elems a
  go (Index a i) = Set.unions $ go <$> (a:i)
  go (Extract r _ _) = go r
  go Unconstrained{} = Set.empty
  go r = error $ "dependsNodeRef "++ show r

dependsNode :: Block -> Node -> Set Id
dependsNode block node = Set.unions $ dependsType block (typeNode node) : case node of
  Apply _ args _ -> map (dependsNodeRef block) args
  Array sh lam _ -> map (d . fst) sh ++ map (d . snd) sh ++ [dependsLambda block lam]
  FoldScan _ _ lam seed ls _ -> [d seed, d ls, dependsLambda block lam]
  Case hd alts _ -> d hd : (dependsLambda' block <$> alts)
  _ -> error $ "dependsNode "++ show node
  where d = dependsNodeRef block

dependsLambda :: Block -> Lambda NodeRef -> Set Id
dependsLambda block (Lambda body ret) = dependsLambda' block (Lambda body [ret])
dependsLambda' :: Block -> Lambda [NodeRef] -> Set Id
dependsLambda' block (Lambda body rets) =
  Set.filter ((dagLevel body >) . idLevel) . Set.unions $
  dependsNodeRef (deriveBlock body block) <$> rets

dependsType :: Block -> Type -> Set Id
dependsType block = go where
  go IntT = Set.empty
  go RealT = Set.empty
  go (SubrangeT t a b) = go t `Set.union` f a `Set.union` f b
    where f Nothing = Set.empty
          f (Just r) = dependsNodeRef block r
  go (ArrayT _ sh t) = Set.unions $
    go t : [dependsNodeRef block a `Set.union` dependsNodeRef block b | (a,b) <- sh]
  go (TupleT ts) = Set.unions $ go <$> ts
  go (UnionT tss) = Set.unions [go t | ts <- tss, t <- ts]
  go UnknownType = Set.empty

dependsD :: DExpr -> Set Id
dependsD e = Set.filter (not . isInternal) $ dependsNodeRef block ret
  where (ret, block) = runDExpr e

getNextLevel :: State Block Level
getNextLevel =
  gets nextLevel

condD :: [(DExpr,DExpr)] -> DExpr
condD cvs = DExpr $ do
  is <- mapM fromDExpr cs
  js <- mapM fromDExpr vs
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
        liftCond (Cond cvs _:rest) =
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
simplify (Apply f js t) | isCond `any` js, f `notElem` ["ifThenElse","id","log_det"] = do
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
simplify (Apply "/" [Const c _,_] t) | isZeros c = return $ Const c t
simplify (Apply "*" [Const 1 _,x] _) = return x
simplify (Apply "*" [x,Const 1 _] _) = return x
simplify (Apply "/" [x,Const 1 _] _) = return x
simplify (Apply "<>" [Const c _,_] t) | isZeros c = return $ Const c t
simplify (Apply "<>" [_,Const c _] t) | isZeros c = return $ Const c t
simplify (Apply "+" [Const 0 _,x] _) = return x
simplify (Apply "+" [x,Const 0 _] _) = return x
simplify (Apply "+" [Const c _,x] _) | isZeros c, not $ isScalar (typeRef x) = return x
simplify (Apply "+" [x,Const c _] _) | isZeros c, not $ isScalar (typeRef x) = return x
simplify (Apply "-" [x,Const 0 _] _) = return x
simplify (Apply "*" [Const (-1) _,x] t) = simplify (Apply "negate" [x] t)
simplify (Apply "*" [x,Const (-1) _] t) = simplify (Apply "negate" [x] t)
simplify (Apply "-" [Const 0 _,x] t) = simplify (Apply "negate" [x] t)
simplify (Apply f js t) | f `elem` ["+","+s","-","*","/","==","/=","<","<=",">",">="
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
simplify (Apply "replaceIndex" [Const 0 _,_,Const 0 _] t) = return $ Const 0 t
simplify (Apply "vectorSize" [v] _) | ArrayT _ [(Const 1 _,n)] _ <- typeRef v = return n
simplify (Apply s js t) | s == "+" || s == "+s" = do
  block <- get
  case simplifySum block js of
    [a] -> return a
    [a,b] -> liftcons (Apply "+" [a,b] t)
    js' -> liftcons (Apply "+s" js' t)
  where simplifySum block refs = sort $ do
          ref <- refs
          case ref of
            Const c _ | isZeros c -> mzero
            Var i@Internal{} _ | Apply f js _ <- lookupBlock i block
                              , f `elem` ["+","+s"] -> simplifySum block js
            _ -> return ref
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
simplify' block (Apply "log_det" [Var i@Internal{} _] t)
  | (Apply "eye" _ _) <- lookupBlock i block = Right . return $ Const 0 t
simplify' block (Apply "log_det" [Var i@Internal{} _] t)
  | (Apply "*" [a,b] _) <- lookupBlock i block, isScalar (typeRef a) = Right $ do
      loga <- simplify $ Apply "log" [a] t
      logb <- simplify $ Apply "log_det" [b] t
      simplify $ Apply "+" [loga, logb] t
simplify' block (Apply "log_det" [Var i@Internal{} _] t)
  | (Apply "/" [a,b] _) <- lookupBlock i block, isScalar (typeRef b) = Right $ do
      loga <- simplify $ Apply "log_det" [a] t
      logb <- simplify $ Apply "log" [b] t
      simplify $ Apply "-" [loga, logb] t
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
    n | ArrayT (Just "matrix") _ _ <- typeRef m -> trace ("matrixRowsCols "++ show n) $
      simplify $ Apply (if cols then "matrixCols" else "matrixRows") [m] IntT
matrixRowsCols cols m | ArrayT (Just "matrix") _ _ <- typeRef m =
  simplify $ Apply (if cols then "matrixCols" else "matrixRows") [m] IntT
matrixRowsCols _ r = error $ "matrixRowsCols "++ show r

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

apply :: String -> Type -> [Expression a] -> Expression r
apply f t = Expression . apply' f t . map erase
apply' :: String -> Type -> [DExpr] -> DExpr
apply' f t xs = DExpr $ do
    js <- mapM fromDExpr xs
    simplify $ Apply f js t

apply2 :: String -> Type -> Expression a -> Expression b -> Expression r
apply2 f t x y = Expression $ apply2' f t (erase x) (erase y)
apply2' :: String -> Type -> DExpr -> DExpr -> DExpr
apply2' f t x y = DExpr $ do
    i <- fromDExpr x
    j <- fromDExpr y
    simplify $ Apply f [i,j] t

applyClosed1 :: String -> Expression a -> Expression a
applyClosed1 f = Expression . applyClosed1' f . erase
applyClosed1' :: String -> DExpr -> DExpr
applyClosed1' f x = DExpr $ do
    i <- fromDExpr x
    simplify $ Apply f [i] (typeRef i)

applyClosed2 :: String -> Expression a -> Expression a -> Expression a
applyClosed2 f x y = Expression $ applyClosed2' f (erase x) (erase y)
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
    d <- getNextLevel
    let ids = [(Dummy d i, IntT) | i <- [1..length sh]]
        e = ar ! [DExpr . return $ Var i t | (i,t) <- ids]
        slam = runLambda ids (fromDExpr e)
    if not $ stickNode dag (Array sh (evalState slam block) UnknownType)
    then liftBlock $ floatArray l ar
    else do
      Lambda dag ret <- slam
      hashcons . Array sh (Lambda dag ret) $ ArrayT l sh (typeRef ret)

stickNode :: DAG -> Node -> Bool
stickNode dag a = case a of
  Apply{} -> True
  Array sh lam _ ->
    varies dag (map fst sh ++ map snd sh) || variesLambda dag lam
  FoldScan _ _ lam sd ls _ ->
    varies dag [sd,ls] || variesLambda dag lam
  Case hd alts _ ->
    varies dag [hd] || variesLambda' dag `any` alts
  Unfold lam sd _ ->
    varies dag [sd] || variesLambda dag lam
  Function lam _ ->
    variesLambda dag lam

floatNode :: Node -> State Block NodeRef
floatNode Apply{} = error "floatNode Apply"
floatNode a = do
    block <- get
    let dag = topDAG block
    if stickNode dag a then hashcons a
    else liftBlock $ floatNode a

array' :: Maybe String -> Int -> AA.AbstractArray DExpr DExpr -> DExpr
array' l n a = if length sh == n
                then DExpr $ floatArray l a
                else error "dimension mismatch"
  where sh = AA.shape a
array :: (Num i, ExprType i, ExprType e)
      => Maybe String -> Int -> AA.AbstractArray (Expression i) (Expression e) -> Expression t
array l n = Expression . array' l n . eraseAA

eraseAA :: AA.AbstractArray (Expression i) (Expression e) -> AA.AbstractArray DExpr DExpr
eraseAA arr = f' <$> AA.fromShape sh'
  where sh' = flip map (AA.shape arr) $ \(a,b) -> (erase a, erase b)
        f' = erase . (arr!) . map Expression

index :: Expression a -> [Expression i] -> Expression r
index a es = expr $ do
    f <- fromExpr a
    js <- mapM fromExpr es
    return $ Index f js

-- | Stochaskell equivalent of 'Prelude.foldl'
foldlE :: (ExprType b) =>
  (Expression b -> Expression a -> Expression b) -> Expression b -> Expression [a] -> Expression b
foldlE f r xs = expr $ foldscan Fold Left_ (flip f) r xs

-- | Stochaskell equivalent of 'Prelude.foldr'
foldrE :: (ExprType b) =>
  (Expression a -> Expression b -> Expression b) -> Expression b -> Expression [a] -> Expression b
foldrE f r xs = expr $ foldscan Fold Right_ f r xs

-- | Stochaskell equivalent of 'Prelude.scanl'
scanlE :: (ExprType b) =>
  (Expression b -> Expression a -> Expression b) -> Expression b -> Expression [a] -> Expression [b]
scanlE f r xs = expr $ foldscan Scan Left_ (flip f) r xs

-- | Stochaskell equivalent of 'Prelude.scanr'
scanrE :: (ExprType b) =>
  (Expression a -> Expression b -> Expression b) -> Expression b -> Expression [a] -> Expression [b]
scanrE f r xs = expr $ foldscan Scan Right_ f r xs

--scan :: (ExprType b) =>
--  (Expression b -> Expression a -> Expression b) -> Expression b -> Expression [a] -> Expression [b]
--scan f r xs = expr $ foldscan ScanRest Left_ (flip f) r xs

foldscan :: forall a b. (ExprType b) => FoldOrScan -> LeftRight ->
  (Expression a -> Expression b -> Expression b) -> Expression b -> Expression [a] -> State Block NodeRef
foldscan fs dir f r xs = do
    seed <- fromExpr r
    l <- fromExpr xs
    block <- get
    let d = nextLevel block
        (ArrayT _ [(Const 1 IntT,n)] _) = typeRef l
        s = typeIndex 1 $ typeRef l
        TypeIs t = typeOf :: TypeOf b
        i = expr . return $ Var (Dummy d 11) s
        j = expr . return $ Var (Dummy d 12) t
        -- TODO: runLambda
        (ret, Block (dag:block')) = runState (fromExpr $ f i j) $
          deriveBlock (DAG d [(Dummy d 11,s), (Dummy d 12,t)] Bimap.empty) block
    if varies (topDAG block) [seed,l] ||
       (not . null $ inputs (topDAG block) `intersect` collectIds dag)
      then do
        put $ Block block'
        case fs of
          Fold ->
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t
          Scan -> do
            n1 <- simplify $ Apply "+" [n, Const 1 IntT] IntT
            let t' = ArrayT Nothing [(Const 1 IntT, n1)] t
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t'
          ScanRest -> do
            let t' = ArrayT Nothing [(Const 1 IntT, n)] t
            hashcons $ FoldScan fs dir (Lambda dag ret) seed l t'
      else liftBlock $ foldscan fs dir f r xs

-- | @find p d v@: find leftmost element of @v@ satisfying @p@,
-- else @d@ if no elements satisfy @p@
find' :: (ExprType e) => (Expression e -> B) -> Expression e -> Expression [e] -> Expression e
find' p = foldrE $ \i -> ifB (p i) i

-- | Stochaskell equivalent of 'Prelude.sum'
sum' :: (ExprType a, Num a) => Expression [a] -> Expression a
sum' = foldlE (+) 0

-- | Stochaskell equivalent of 'Prelude.floor'
floor' :: R -> Z
floor' x = apply "floor" IntT [x]

-- | given a value and a sorted vector, find the index at which we could
-- 'Data.Array.Abstract.insertAt' the value whilst keeping the vector sorted
findSortedInsertIndex :: R -> RVec -> Z
findSortedInsertIndex = apply2 "findSortedInsertIndex" IntT

-- | minimum of two values
min' :: Expression a -> Expression a -> Expression a
min' = applyClosed2 "min"

-- | Stochaskell equivalent of 'trace' that runs within generated code
debug :: (ExprTuple t) => String -> t -> t
debug msg t = toExprTuple $ do
  e <- fromExprTuple t
  return . DExpr $ do
    _ <- fromDExpr $ apply' ("debug$"++ msg) (TupleT []) (fromExprTuple t)
    fromDExpr e


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

instance (ExprType t, Num t) => Num (Expression t) where
    (+) = applyClosed2 "+"
    (-) = applyClosed2 "-"
    (*) = applyClosed2 "*"

    negate = applyClosed1 "negate"
    abs    = applyClosed1 "abs"
    signum = applyClosed1 "signum"

    fromInteger  = expr . return . flip Const t . fromInteger
      where TypeIs t = typeOf :: TypeOf t

instance (ExprType t, Fractional t) => Fractional (Expression t) where
    fromRational = expr . return . flip Const t . fromRational
      where TypeIs t = typeOf :: TypeOf t
    (/) = applyClosed2 "/"

instance (ExprType t, Floating t) => Floating (Expression t) where
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

instance (ExprType e) => AA.Vector (Expression [e]) Z (Expression e) where
    vector = array (Just "vector") 1
    blockVector = Expression . AA.blockVector . map erase
    vectorSize  = Expression . AA.vectorSize . erase
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
  r <- if saneRow `all` k
       then simplify $ Apply "+s" rs IntT
       else return $ Unconstrained UnknownType
  c <- if saneCol `all` transpose k
       then simplify $ Apply "+s" cs IntT
       else return $ Unconstrained UnknownType
  let t = ArrayT (Just "matrix") [(Const 1 IntT,r),(Const 1 IntT,c)] $
        coerces [t' | ArrayT _ _ t' <- typeRef <$> concat k]
  return . flip BlockArray t $ A.array bnd
    [([i,j], k !! (i-1) !! (j-1)) | [i,j] <- A.range bnd]
  where flattenRow = foldr1 (zipWith (++))
        flattenMatrix = concatMap flattenRow :: [[[[a]]]] -> [[a]]
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
        sane = allSame (map length k)
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

instance (ExprType e) => AA.Matrix (Expression [[e]]) Z (Expression e) where
    matrix = array (Just "matrix") 2
    blockMatrix m = Expression $ AA.blockMatrix (fmap erase <$> m)
    eye = Expression . AA.eye . erase
    zeros m n = Expression $ AA.zeros (erase m) (erase n)
instance AA.Matrix DExpr DExpr DExpr where
    matrix = array' (Just "matrix") 2
    blockMatrix m = DExpr $ do
      k' <- sequence $ mapM asMatrixD <$> m
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
      let t = ArrayT (Just "matrix") [(Const 1 IntT,n')
                                     ,(Const 1 IntT,n')] RealT
      simplify $ Apply "eye" [n'] t
    zeros m n = DExpr $ do
      m' <- fromDExpr m
      n' <- fromDExpr n
      let t = ArrayT (Just "matrix") [(Const 1 IntT,m')
                                     ,(Const 1 IntT,n')] RealT
      simplify $ Apply "zeros" [m',n'] t

typeMatrixProduct (ArrayT _ [r,_] t) (ArrayT _ [_,c] _) = ArrayT (Just "matrix") [r,c] t

instance (ExprType e) => Semigroup (Expression [[e]]) where
    a <> b = Expression $ erase a <> erase b
instance Semigroup DExpr where
    a <> b = DExpr $ do
        i <- fromDExpr a
        j <- fromDExpr b
        simplify . Apply "<>" [i,j] $ typeRef i `typeMatrixProduct` typeRef j

instance AA.Indexable (Expression [e]) (Expression Integer) (Expression e) where
    a ! e = Expression $ erase a ! erase e
    deleteIndex  a e   = Expression $ AA.deleteIndex  (erase a) (erase e)
    insertIndex  a e d = Expression $ AA.insertIndex  (erase a) (erase e) (erase d)
    replaceIndex a e d = Expression $ AA.replaceIndex (erase a) (erase e) (erase d)
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
instance (ExprType e) => AA.Scalable (Expression e) (Expression [[e]]) where
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
instance (ExprType e) => LA.Transposable (Expression [[e]]) (Expression [[e]]) where
    tr  = Expression . LA.tr  . erase
    tr' = Expression . LA.tr' . erase

instance AA.InnerProduct (Expression [e]) (Expression e) where
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
    m <\> v = DExpr $ do
        i <- fromDExpr m
        j <- fromDExpr v
        let (ArrayT _ [_,c] t) = typeRef i
            t' = case typeRef j of
              ArrayT _ [_]   _ -> ArrayT (Just "vector") [c] t
              ArrayT _ [_,n] _ -> ArrayT (Just "matrix") [c,n] t
        simplify $ Apply "<\\>" [i,j] t'
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
instance AA.LinearOperator (Expression [[e]]) (Expression [e]) where
    m #> v   = Expression $ erase m AA.#>  erase v
    v <# m   = Expression $ erase v AA.<#  erase m
    m <\> v  = Expression $ erase m AA.<\> erase v
    diag     = Expression . AA.diag     . erase
    asColumn = Expression . AA.asColumn . erase
    asRow    = Expression . AA.asRow    . erase

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
          _ -> let (ArrayT _ _ t) = typeRef i
               in simplify $ Apply "log_det" [i] t
instance AA.SquareMatrix (Expression [[e]]) (Expression e) where
    chol   = Expression . AA.chol   . erase
    inv    = Expression . AA.inv    . erase
    det    = Expression . AA.det    . erase
    logDet = Expression . AA.logDet . erase

-- | \(\mathrm{qfDiag}\ A\ x = x^\top A x\)
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
instance Boolean (Expression Bool) where
    true  = apply "true" boolT []
    false = apply "false" boolT []
    notB  = applyClosed1 "not"
    (&&*) = applyClosed2 "&&"
    (||*) = applyClosed2 "||"

type instance BooleanOf DExpr = DExpr
type instance BooleanOf (Expression t) = Expression Bool

instance IfB (Expression t) where
  ifB c x y = Expression $ ifB (erase c) (erase x) (erase y)
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
instance EqB (Expression t) where
    (==*) = apply2 "==" boolT
    (/=*) = apply2 "/=" boolT

instance OrdB (Expression t) where
    (<*)  = apply2 "<"  boolT
    (<=*) = apply2 "<=" boolT
    (>=*) = apply2 ">=" boolT
    (>*)  = apply2 ">"  boolT

instance (Ord t, ExprType t) => Transfinite (Expression t) where
    infinity = constExpr infinity

detuple :: (ExprTuple t) => t -> Expression t
detuple t = expr $ do
  js <- sequence $ fromDExpr <$> fromExprTuple t
  return $ case js of
    [j] -> j
    _ -> Data 0 js . TupleT $ typeRef <$> js

entuple :: forall t. (ExprTuple t) => Expression t -> t
entuple = toExprTuple . entupleD n . erase
  where TupleSize n = tupleSize :: TupleSize t
entupleD :: Int -> DExpr -> [DExpr]
entupleD 1 e = [e]
entupleD n e = extractD e 0 <$> [0..(n-1)]

newtype TupleSize t = TupleSize Int
newtype TypesOf t = TypesIs [Type]
-- | Stochaskell interface for tuples of expressions
class ExprType t => ExprTuple t where
    -- | numer of tuple elements
    tupleSize :: TupleSize t
    -- | convert to list of expressions
    fromExprTuple :: t -> [DExpr]
    -- | convert from list of expressions
    toExprTuple :: [DExpr] -> t
    -- | convert from list of constants
    fromConstVals :: [ConstVal] -> t
    -- | list of types of elements
    typesOf :: TypesOf t

zipExprTuple :: (ExprTuple t) => t -> t -> [(DExpr,DExpr)]
zipExprTuple s t = fromExprTuple s `zip` fromExprTuple t

constDExpr :: ConstVal -> Type -> DExpr
constDExpr c = DExpr . return . Const c

instance forall a. (ExprType a) => ExprTuple (Expression a) where
    tupleSize = TupleSize 1
    fromExprTuple a = [erase a]
    toExprTuple [a] = Expression a
    fromConstVals [a] = constExpr a
    typesOf = TypesIs [t] where
      TypeIs t = typeOf :: TypeOf a
instance (ExprType a, ExprType b) =>
         ExprTuple (Expression a, Expression b) where
    tupleSize = TupleSize 2
    fromExprTuple (a,b) = [erase a, erase b]
    toExprTuple [a,b] = (Expression a, Expression b)
    fromConstVals [a,b] = (constExpr a, constExpr b)
    typesOf = TypesIs [s,t] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
instance (ExprType a, ExprType b, ExprType c) =>
         ExprTuple (Expression a, Expression b, Expression c) where
    tupleSize = TupleSize 3
    fromExprTuple (a,b,c) = [erase a, erase b, erase c]
    toExprTuple [a,b,c] = (Expression a, Expression b, Expression c)
    fromConstVals [a,b,c] = (constExpr a, constExpr b, constExpr c)
    typesOf = TypesIs [s,t,u] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
instance (ExprType a, ExprType b, ExprType c, ExprType d) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d) where
    tupleSize = TupleSize 4
    fromExprTuple (a,b,c,d) = [erase a, erase b, erase c, erase d]
    toExprTuple [a,b,c,d] = (Expression a, Expression b, Expression c, Expression d)
    fromConstVals [a,b,c,d] = (constExpr a, constExpr b, constExpr c, constExpr d)
    typesOf = TypesIs [s,t,u,v] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
      TypeIs v = typeOf :: TypeOf d
instance (ExprType a, ExprType b, ExprType c, ExprType d,
          ExprType e) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d, Expression e) where
    tupleSize = TupleSize 5
    fromExprTuple (a,b,c,d,e) =
      [erase a, erase b, erase c, erase d, erase e]
    toExprTuple [a,b,c,d,e] =
      (Expression a, Expression b, Expression c, Expression d, Expression e)
    fromConstVals [a,b,c,d,e] =
      (constExpr a, constExpr b, constExpr c, constExpr d, constExpr e)
    typesOf = TypesIs [s,t,u,v,w] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
      TypeIs v = typeOf :: TypeOf d
      TypeIs w = typeOf :: TypeOf e
instance (ExprType a, ExprType b, ExprType c, ExprType d,
          ExprType e, ExprType f) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f) where
    tupleSize = TupleSize 6
    fromExprTuple (a,b,c,d,e,f) =
      [erase a, erase b, erase c, erase d, erase e, erase f]
    toExprTuple [a,b,c,d,e,f] =
      (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f)
    fromConstVals [a,b,c,d,e,f] =
      (constExpr a, constExpr b, constExpr c, constExpr d, constExpr e, constExpr f)
    typesOf = TypesIs [s,t,u,v,w,x] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
      TypeIs v = typeOf :: TypeOf d
      TypeIs w = typeOf :: TypeOf e
      TypeIs x = typeOf :: TypeOf f
instance (ExprType a, ExprType b, ExprType c, ExprType d,
          ExprType e, ExprType f, ExprType g) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g) where
    tupleSize = TupleSize 7
    fromExprTuple (a,b,c,d,e,f,g) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g]
    toExprTuple [a,b,c,d,e,f,g] =
      (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g)
    fromConstVals [a,b,c,d,e,f,g] =
      (constExpr a, constExpr b, constExpr c, constExpr d, constExpr e, constExpr f, constExpr g)
    typesOf = TypesIs [s,t,u,v,w,x,y] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
      TypeIs v = typeOf :: TypeOf d
      TypeIs w = typeOf :: TypeOf e
      TypeIs x = typeOf :: TypeOf f
      TypeIs y = typeOf :: TypeOf g
instance (ExprType a, ExprType b, ExprType c, ExprType d,
          ExprType e, ExprType f, ExprType g, ExprType h) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h) where
    tupleSize = TupleSize 8
    fromExprTuple (a,b,c,d,e,f,g,h) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g, erase h]
    toExprTuple [a,b,c,d,e,f,g,h] =
      (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h)
    fromConstVals [a,b,c,d,e,f,g,h] =
      (constExpr a, constExpr b, constExpr c, constExpr d, constExpr e, constExpr f, constExpr g, constExpr h)
    typesOf = TypesIs [s,t,u,v,w,x,y,z] where
      TypeIs s = typeOf :: TypeOf a
      TypeIs t = typeOf :: TypeOf b
      TypeIs u = typeOf :: TypeOf c
      TypeIs v = typeOf :: TypeOf d
      TypeIs w = typeOf :: TypeOf e
      TypeIs x = typeOf :: TypeOf f
      TypeIs y = typeOf :: TypeOf g
      TypeIs z = typeOf :: TypeOf h
instance (ExprType a, ExprType b, ExprType c, ExprType d,
          ExprType e, ExprType f, ExprType g, ExprType h, ExprType i) =>
         ExprTuple (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h, Expression i) where
    tupleSize = TupleSize 9
    fromExprTuple (a,b,c,d,e,f,g,h,i) =
      [erase a, erase b, erase c, erase d, erase e, erase f, erase g, erase h, erase i]
    toExprTuple [a,b,c,d,e,f,g,h,i] =
      (Expression a, Expression b, Expression c, Expression d, Expression e, Expression f, Expression g, Expression h, Expression i)
    fromConstVals [a,b,c,d,e,f,g,h,i] =
      (constExpr a, constExpr b, constExpr c, constExpr d, constExpr e, constExpr f, constExpr g, constExpr h, constExpr i)
    typesOf = TypesIs [r,s,t,u,v,w,x,y,z] where
      TypeIs r = typeOf :: TypeOf a
      TypeIs s = typeOf :: TypeOf b
      TypeIs t = typeOf :: TypeOf c
      TypeIs u = typeOf :: TypeOf d
      TypeIs v = typeOf :: TypeOf e
      TypeIs w = typeOf :: TypeOf f
      TypeIs x = typeOf :: TypeOf g
      TypeIs y = typeOf :: TypeOf h
      TypeIs z = typeOf :: TypeOf i

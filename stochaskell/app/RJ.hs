{-# LANGUAGE FlexibleContexts, MonadComprehensions, RebindableSyntax #-}

module Main where
import Language.Stochaskell

import Control.Monad hiding (guard)
import Control.Monad.HT (iterateLimit)
import Data.Either.Utils
import qualified Data.Expression as E
import Data.Expression.Eval
import qualified Data.Map.Strict as Map

data Model = Model1 R ZVec
           | Model2 R R ZVec
           deriving (Show)

instance Constructor Model where
  tags = Tags [0..1]
  construct f 0 [lam,    y] = Model1 (f lam)         (f y)
  construct f 1 [lam,kap,y] = Model2 (f lam) (f kap) (f y)
  deconstruct f (Model1 lam     y) = (0, [f lam,        f y])
  deconstruct f (Model2 lam kap y) = (1, [f lam, f kap, f y])
  typeUnion m = do
    t <- typeExpr y
    return $ UnionT [[RealT, t] ,[RealT, RealT, t]]
    where y = case m of
                Model1 _   y -> y
                Model2 _ _ y -> y

instance ScalarType Model where
  fromConcrete = fromConcreteC
  toConcrete = toConcreteC
  constVal = fromRight . eval_ . fromConcrete

model1 :: R -> ZVec -> Expr Model
model1 lam y = fromConcrete (Model1 lam y)
model2 :: R -> R -> ZVec -> Expr Model
model2 lam kap y = fromConcrete (Model2 lam kap y)

pvnb1 :: Z -> P (R,ZVec)
pvnb1 n = do
  lam <- gamma 25 10
  y <- joint vector [ poisson lam | _ <- 1...n ]
  return (lam,y)

pvnb2 :: Z -> P (R,R,ZVec)
pvnb2 n = do
  lam <- gamma 25 10
  kap <- gamma 1 10
  let a = 1/kap
      b = a/lam
  y <- joint vector [ negBinomial a b | _ <- 1...n ]
  return (lam,kap,y)

pvnb :: Z -> P (Expr Model)
pvnb n = do
  (lam1,     y1) <- pvnb1 n
  (lam2,kap2,y2) <- pvnb2 n
  k <- bernoulli 0.5
  return $ if k then model1 lam1 y1 else model2 lam2 kap2 y2

pvnbJump :: Model -> P (Expr Model)
pvnbJump (Model1 lam y) = do
  u <- normal 0 1.5
  let kap = 0.015 * exp u
  return (model2 lam kap y)
pvnbJump (Model2 lam _ y) =
  return (model1 lam y)

pvnbHMC :: Z -> Model -> IO Model
pvnbHMC n (Model1 lam y) = do
  lam' <- last <$> hmcStanInit 10 [ lam' | (lam',y') <- pvnb1 n, y' == y ] lam
  return (Model1 lam' y)
pvnbHMC n (Model2 lam kap y) = do
  (lam',kap') <- last <$> hmcStanInit 10
    [ (lam',kap') | (lam',kap',y') <- pvnb2 n, y' == y ] (lam,kap)
  return (Model2 lam' kap' y)

pvnbStep :: Z -> Model -> IO Model
pvnbStep n m = do
  m' <- pvnbHMC n m
  m'' <- pvnb n `rjmcC` pvnbJump `runCC` fromConcrete m'
  return . fromRight $ eval' m''

mainPVNB :: IO ()
mainPVNB = do
  let n = 100
  (_,y) <- simulate (pvnb1 n)
  samples <- tail <$> iterateLimit 5000 (pvnbStep n) (Model1 1 y)
  forM_ samples $ \m -> case m of
    Model1 lam _ -> putStrLn $ "q11 "++ show lam
    Model2 lam kap _ -> do
      putStrLn $ "q21 "++ show lam
      putStrLn $ "q22 "++ show kap
  print $ fromIntegral (length $ filter isModel1 samples) / fromIntegral (length samples)
  where isModel1 Model1{} = True
        isModel1 _ = False

---

coalData :: [Double]
coalData =
  [ 1851.203, 1851.632, 1851.969, 1851.975, 1852.314, 1852.347, 1852.358, 1852.385, 1852.977
  , 1853.196, 1853.229, 1853.319, 1853.500, 1854.135, 1856.396, 1856.506, 1856.539, 1856.618
  , 1857.138, 1857.404, 1857.582, 1858.091, 1858.154, 1858.406, 1858.945, 1860.125, 1860.169
  , 1860.591, 1860.851, 1860.919, 1860.971, 1861.185, 1861.738, 1861.836, 1862.138, 1862.893
  , 1862.937, 1863.178, 1863.794, 1863.939, 1863.986, 1865.459, 1865.971, 1866.064, 1866.340
  , 1866.452, 1866.833, 1866.948, 1866.951, 1867.635, 1867.854, 1867.862, 1868.749, 1868.903
  , 1868.988, 1869.251, 1869.442, 1869.554, 1869.809, 1869.875, 1870.124, 1870.515, 1870.559
  , 1870.633, 1871.027, 1871.151, 1871.167, 1871.736, 1871.816, 1872.123, 1872.240, 1872.769
  , 1873.136, 1874.285, 1874.546, 1874.888, 1874.981, 1875.329, 1875.925, 1875.931, 1875.931
  , 1876.966, 1877.064, 1877.105, 1877.190, 1877.779, 1877.809, 1878.184, 1878.195, 1878.236
  , 1878.433, 1878.696, 1879.036, 1879.172, 1879.501, 1880.057, 1880.539, 1880.689, 1880.944
  , 1881.105, 1881.968, 1882.129, 1882.296, 1882.299, 1882.335, 1882.852, 1883.797, 1883.851
  , 1884.073, 1884.856, 1885.168, 1885.464, 1885.979, 1886.617, 1886.693, 1886.754, 1886.921
  , 1887.134, 1887.405, 1888.298, 1889.051, 1889.198, 1889.793, 1890.102, 1890.190, 1891.252
  , 1891.665, 1892.654, 1893.508, 1894.477, 1895.318, 1896.070, 1896.284, 1896.331, 1899.630
  , 1901.393, 1902.671, 1905.056, 1905.188, 1905.524, 1906.773, 1908.136, 1908.270, 1908.629
  , 1909.127, 1909.825, 1910.357, 1910.970, 1912.520, 1913.784, 1914.409, 1916.615, 1918.031
  , 1922.529, 1922.677, 1923.569, 1927.162, 1928.114, 1930.154, 1930.748, 1931.077, 1931.830
  , 1931.884, 1932.065, 1932.864, 1932.875, 1933.883, 1934.723, 1935.643, 1935.695, 1936.596
  , 1937.500, 1938.354, 1939.821, 1940.218, 1940.424, 1941.420, 1941.522, 1941.574, 1942.001
  , 1942.129, 1942.483, 1946.945, 1947.025, 1947.619, 1947.638, 1947.687, 1951.405, 1957.883
  , 1960.489, 1962.220]

coalData' :: [Double]
coalData' = subtract (head days) <$> days
  where days = [365.2425 * yr | yr <- coalData]

coalPrior :: R -> P (Z,RVec,RVec)
coalPrior t = do
  let lam = 3; nMax = 30; a = 1; b = 200
  n <- truncated 1 nMax $ poisson lam
  s' <- orderedSample (2*n + 1) (uniform 0 t) :: P RVec
  let s = vector [ s'!(2*i) | i <- 1...n ]
  g <- joint vector [ gamma a b | _ <- 1...(n+1) ]
  return (n,s,g)

coalLikelihood :: R -> (Z,RVec,RVec) -> P RVec
coalLikelihood t (n,s,g) = do
  let rate y = let j = findSortedInsertIndex y s in (g!j)
      widths = vector [(s!(i+1)) - (s!i) | i <- 1...(n-1)] :: RVec
      w = blockVector [cast (s!1), widths, cast $ t - (s!n)] :: RVec
      mean = E.foldl (+) 0 $ vector [(w!j) * (g!j) | j <- 1...(n+1) ]
  y <- poissonProcess t rate mean
  return y

type CoalModel = (Z,RVec,RVec,RVec)
coal :: R -> P CoalModel
coal t = do
  (n,s,g) <- coalPrior t
  y <- coalLikelihood t (n,s,g)
  return (n,s,g,y)

coalMoveH :: R -> CoalModel -> P CoalModel
coalMoveH t (n,s,g,y) = do
  j <- uniform 1 (n+1)
  u <- uniform (-1/2) (1/2)
  let g' = g `replaceAt` (j, exp (log (g!j) + u))
  return (n,s,g',y)

coalMoveP :: R -> CoalModel -> P CoalModel
coalMoveP t (n,s_,g,y) = do
  let s i = if i == 0 then 0 else if i == (n+1) then t else (s_!i)
  j <- uniform 1 n
  u <- uniform 0 1
  let sj' = s (j-1) + (s (j+1) - s (j-1)) * u
  let s' = s_ `replaceAt` (j, sj')
  return (n,s',g,y)

coalMoveBirth :: R -> CoalModel -> P CoalModel
coalMoveBirth t (n,s_,g,y) = do
  let s i = if i == 0 then 0 else if i == (n+1) then t else (s_!i)
  x <- uniform 0 t
  let j = find' (\j -> x < s j) (n+1) $ vector (1...n)
      i = j-1
      k = j+1
      s' = s_ `insertAt` (j, x)
  u <- uniform 0 1
  let gj' = (g!j) * (u / (1-u))**((s j - x) / (s j - s i))
      gk' = (g!j) * ((1-u) / u)**((x - s i) / (s j - s i))
      g' = g `replaceAt` (j, gj') `insertAt` (k, gk')
  return (n+1,s',g',y)

coalMoveDeath :: R -> CoalModel -> P CoalModel
coalMoveDeath t (n,s_,g,y) = do
  let s i = if i == 0 then 0 else if i == (n+1) then t else (s_!i)
  j <- uniform 1 n
  let s' = s_ `deleteAt` j
      i = j-1
      k = j+1
      gj' = (g!j)**((s j - s i) / (s k - s i)) *
            (g!k)**((s k - s j) / (s k - s i))
      g' = g `deleteAt` k `replaceAt` (j, gj')
  return (n-1,s',g',y)

coalMoveBirth' :: R -> CoalModel -> P CoalModel
coalMoveBirth' t (n,s,g,y) = do
  x <- uniform 0 t
  let j = findSortedInsertIndex x s
      s' = s `insertAt` (j, x)
  u <- lognormal 0 1
  let g' = g `insertAt` (j+1, u * (g!j))
  return (n+1,s',g',y)

coalMoveDeath' :: R -> CoalModel -> P CoalModel
coalMoveDeath' t (n,s,g,y) = do
  j <- uniform 1 n
  let s' = s `deleteAt` j
      g' = g `deleteAt` (j+1)
  return (n-1,s',g',y)

coalMoveProbs :: Z -> (R,R,R,R)
coalMoveProbs n = (e,p,b,d)
  where lam = 3
        nMax = 30
        c = 0.9 * (real lam + 1) / (2 * real lam + 1)
        b = if n == nMax then 0 else if n < (lam - 1) then c else c * real lam / (cast n + 1)
        d = if n == 1    then 0 else if lam < n       then c else c * cast n / real lam
        p = if n == 1 then 0     else (1-b-d)/2
        e = if n == 1 then 1-b-d else (1-b-d)/2

coalMove :: R -> CoalModel -> P CoalModel
coalMove t m@(n,_,_,_) = do
  let (e,p,b,d) = coalMoveProbs n
  mixture' [(e, coalMoveH t m)
           ,(p, coalMoveP t m)
           ,(b, coalMoveBirth t m)
           ,(d, coalMoveDeath t m)
           ]

coalMove' :: R -> CoalModel -> P CoalModel
coalMove' t m@(n,_,_,_) = do
  let (e,p,b,d) = coalMoveProbs n
  mixture' [(e, coalMoveH t m)
           ,(p, coalMoveP t m)
           ,(b, coalMoveBirth' t m)
           ,(d, coalMoveDeath' t m)
           ]

main :: IO ()
main = do
  let t = 40907
  (n0,s0,g0) <- simulate (coalPrior t)
  print (n0,s0,g0)
  print $ coalLikelihood t (n0,s0,g0)
  let ts = [IntT, vecT RealT, vecT RealT, vecT RealT]
      step = coal t `rjmc` coalMove' t $ entuple . expr . return $
        Data 0 [Var (Dummy 9 i) t | (i,t) <- zip [0..] ts] (TupleT ts)
      (stepRets, stepPB) = runProgExprs "sim" step
  putStrLn $ showPBlock stepPB $ show stepRets
  let kernel m = do
        let env = Map.fromList $ (LVar . Dummy 9 <$> [0..3]) `zip` fromRight (evalTuple emptyEnv m)
        m'@(n,s,g,_) <- fromConstVals <$> samplePBlock env stepPB stepRets
        print (n,s,g)
        return m'
      yData = list coalData' :: RVec
  _ <- chain 44000 kernel (n0,s0,g0,yData)
  return ()

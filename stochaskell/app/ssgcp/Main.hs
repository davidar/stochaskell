{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Language.Stochaskell
import Language.Stochaskell.Plot
import System.Directory

prefix = "ssgcp"

-- Equivalent covariance function (m -> infinity):
--   exp (- ((x - y) * ils)^2 / 2)
gpTrigPoly :: R -> Z -> P R -> R -> RVec -> RVec -> R -> R
gpTrigPoly t m sd z a b x = z * amp 0 + sqrt 2 * sum' v where
  amp j = sqrt $ pdf sd (j / (2*t)) / (2*t)
  v = vector [ let w = 2*pi * x * cast j / (2*t)
               in amp (cast j) * ((a!j) * cos w + (b!j) * sin w)
             | j <- 1...m ]

poissonProcess' :: R -> R -> P (Z,RVec)
poissonProcess' rate t = do
  n <- poisson (rate * t)
  s <- orderedSample n (uniform 0 t)
  return (n,s)

type State = (R,RVec,RVec,R,R,R,Z,RVec,BVec)

dim :: State -> Z
dim (_,_,_,_,_,_,n,_,_) = n

canonicalState :: Z -> State -> State
canonicalState k (z,a,b,eta,ils,cap,n,s,phi) = (z,a,b,eta,ils,cap,n,s',phi)
  where (s1,s0) = integer k `splitAt` list s
        s0' = sort s0 :: [Double]
        s' = list $ s1 ++ s0' :: RVec

ssgcp :: R -> Z -> P State
ssgcp t m = do
  z <- normal 0 1 :: P R
  a <- normals (vector [ 0 | j <- 1...m ]) (vector [ 1 | j <- 1...m ])
  b <- normals (vector [ 0 | j <- 1...m ]) (vector [ 1 | j <- 1...m ])
  eta <- lognormal 0 1
  ils <- lognormal (log 0.1) 1
  cap <- gamma 1 1
  (n,s) <- poissonProcess' cap t
  let sd = normal 0 (ils / (2*pi))
      y i = eta * gpTrigPoly t m sd z a b (s!i)
  phi <- joint vector [ bernoulliLogit (y i) | i <- 1...n ]
  return (z,a,b,eta,ils,cap,n,s,phi)

stepDown' :: Z -> State -> P (Z,Z,RVec,BVec)
stepDown' k (z,a,b,eta,ils,cap,n,s,phi) = do
  i <- uniform (k+1) n
  let n' = n - 1
      s' = s `deleteAt` i
      phi' = phi `deleteAt` i
  return (i,n',s',phi')

stepDown :: Z -> State -> P State
stepDown k state@(z,a,b,eta,ils,cap,n,s,phi) = do
  (i,n',s',phi') <- stepDown' k state
  return (z,a,b,eta,ils,cap,n',s',phi')

stepUp' :: R -> Z -> State -> P (R,Z,RVec,BVec)
stepUp' t k (z,a,b,eta,ils,cap,n,s,phi) = do
  x <- uniform 0 t
  let n' = n + 1
      i = findSortedInsertIndexBound (k+1,n) x s
      s' = s `insertAt` (i, x)
      phi' = phi `insertAt` (i, false)
  return (x,n',s',phi')

stepUp :: R -> Z -> State -> P State
stepUp t k state@(z,a,b,eta,ils,cap,n,s,phi) = do
  (x,n',s',phi') <- stepUp' t k state
  return (z,a,b,eta,ils,cap,n',s',phi')

stepN :: R -> Z -> State -> P State
stepN t k state@(z,a,b,eta,ils,cap,n,s,phi) = mixture'
  [(1/2, stepUp t k state)
  ,(1/2, if n == k then return state
                   else stepDown k state)
  ]

stepS :: R -> Z -> State -> P State
stepS t idx (z,a,b,eta,ils,cap,n,s,phi) = do
  x <- truncated 0 t $ normal (s!idx) (1/ils)
  let s' = s `replaceAt` (idx, x)
  return (z,a,b,eta,ils,cap,n,s',phi)

stepCap :: R -> State -> P State
stepCap t (z,a,b,eta,ils,cap,n,s,phi) = do
  let alpha = cast (1 + n) :: R
  cap' <- gamma alpha (1 + t)
  return (z,a,b,eta,ils,cap',n,s,phi)

sigmoid a = 1 / (1 + exp (-a))

stepN' :: R -> Z -> Z -> State -> P (R,State)
stepN' t m k state@(z,a,b,eta,ils,cap,n,s,phi) = do
  c <- bernoulli 0.5
  if c then do -- birth
    (x,n',s',phi') <- stepUp' t k state
    let state' = (z,a,b,eta,ils,cap,n',s',phi')
        sd = normal 0 (ils / (2*pi))
        y = eta * gpTrigPoly t m sd z a b x
        alpha = t * cap * sigmoid (-y) / cast (n - k + 1)
    return (alpha,state')
  else if n == k then do
    return (1,state)
  else do -- death
    (i,n',s',phi') <- stepDown' k state
    let state' = (z,a,b,eta,ils,cap,n',s',phi')
        sd = normal 0 (ils / (2*pi))
        y = eta * gpTrigPoly t m sd z a b (s!i)
        alpha = cast (n - k) / (t * cap * sigmoid (-y))
    return (alpha,state')

mhN :: R -> Z -> Z -> State -> P State
mhN t m k state = debug "mhN" <$> do
  (alpha,state') <- stepN' t m k state
  let alpha' = rjmc1Ratio (ssgcp t m) (stepN t k) state state'
  accept <- bernoulli (min' 1 $
    0.5 * (debug "N alpha true" alpha + debug "N alpha auto" alpha'))
  return $ if accept then state' else state

mhS :: R -> Z -> Z -> State -> P State
mhS t m idx state@(z,a,b,eta,ils,cap,n,s,phi) = do
  x <- truncated 0 t $ normal (s!idx) (1/ils)
  let s' = s `replaceAt` (idx, x)
      state' = (z,a,b,eta,ils,cap,n,s',phi)
      sd = normal 0 (ils / (2*pi))
      y  = eta * gpTrigPoly t m sd z a b (s!idx)
      y' = eta * gpTrigPoly t m sd z a b x
      alpha = sigmoid (-y') / sigmoid (-y)
      alpha' = rjmc1Ratio (ssgcp t m) (stepS t idx) state state'
  accept <- bernoulli (min' 1 $
    0.5 * (debug "S alpha true" alpha + debug "S alpha auto" alpha'))
  return $ if accept then state' else state

stepMH :: R -> Z -> Z -> State -> P State
stepMH t m k state = do
  state <- chain' 10 (mhN t m k) state
  state <- chainRange' (k + 1, dim state) (mhS t m) state
  state <- stepCap t state
  return state

stepRJ :: R -> Z -> Z -> State -> P State
stepRJ t m k state = do
  state <- chain' 10 (ssgcp t m `rjmc1` stepN t k) state
  state <- chainRange' (k + 1, dim state) (\idx -> ssgcp t m `rjmc1` stepS t idx) state
  state <- stepCap t state
  return state

stepGP :: R -> Z -> State -> IO State
stepGP t m (z,a,b,eta,ils,cap,n,s,phi) = do
  samples <- hmcStanInit 1
    [ (z',a',b',eta',ils')
    | (z',a',b',eta',ils',cap',n',s',phi') <- ssgcp t m,
      cap' == cap, n' == n, s' == s, phi' == phi
    ] (z,a,b,eta,ils)
  let (z',a',b',eta',ils') = last samples
  return (z',a',b',eta',ils',cap,n,s,phi)

genData' :: Double -> IO [Double]
genData' t = do
  let cap = 2 * 5
  n <- poisson (t * cap)
  s <- sequence [ uniform 0 t | _ <- [1..n :: Integer] ]
  let f = [ 2 * exp (-x/15) + exp (-((x-25)/10)^2) | x <- s ]
  phi <- sequence [ bernoulli (y / 2) | y <- f ]
  let dat = s `selectItems` phi
  toSVG (prefix ++"_data") . toRenderable $ do
    plot $ line "truth" [sort $ zip s f]
    plot . points "data" $ zip dat (repeat 1.9)
    plot . points "rejections" $ zip (s `selectItems` map not phi) (repeat 0.1)
  return $ sort dat

initialise :: R -> Z -> [Double] -> IO State
initialise t m dat = do
  let k = integer (length dat)
  z <- real <$> (normal 0 1 :: IO Double)
  a <- fromList <$> sequence [ normal 0 1 | _ <- [1..integer m] ]
  b <- fromList <$> sequence [ normal 0 1 | _ <- [1..integer m] ]
  let eta = 1
      ils = 10 / t
      cap = real (2 * k / t)
      n = k * 2
  rej <- sequence [ uniform 0 (real t) | _ <- [1..(n-k)] ]
  let s = fromList $ dat ++ sort rej :: RVec
      phi = fromList $ replicate k True ++ replicate (n-k) False :: BVec
  return (z,a,b,eta,ils,cap,n,s,phi)

main :: IO ()
main = do
  let t = 50
      m = 20
  --setRandomSeed 3
  dat <- genData' t
  putStrLn $ "data = "++ show dat
  let k = integer (length dat)
      xs = [0.5*x | x <- [0..100]]
  state <- initialise t m dat
  stepMH' <- compileCC $ stepMH t m k
  --let stepMH' = runStep $ stepMH t m k
  createDirectoryIfMissing True (prefix ++"-figs")
  (_, accum) <- flip (chainRange (1,100)) (state,[]) $ \iter (state, accum) -> do
    putStrLn $ "*** CURRENT STATE: "++ show state

    state <- canonicalState k <$> stepMH' state
    let (z,a,b,eta,ils,cap,n,s,phi) = state
    let s' = real <$> list s :: [Double]
        phi' = boolean <$> list phi :: [Bool]
    unless (all (\x -> 0 <= x && x <= t) s') $ error ("s = "++ show s)
    unless (and $ take (integer k) phi') $ error ("phi = "++ show phi)

    state <- stepGP t m state
    let (z,a,b,eta,ils,cap,n,s,phi) = state

    let sd = normal 0 (ils / (2*pi))
        f = (real cap *) . sigmoid . real . (eta *) . gpTrigPoly t m sd z a b . real
        fs = f <$> xs :: [Double]

    toSVG (prefix ++"-figs/"++ show iter) . toRenderable $ do
      plot $ line "rate" [zip xs fs]
      plot . points "data" $ zip (list s) [if y then 0.9 else 0.1 :: Double | y <- toList phi]
    return ((z,a,b,eta,ils,cap,n,s,phi), fs:accum)

  let samples = drop 50 accum
      fmean = mean samples
      fmean2 = mean (map (**2) <$> samples)
      fsd = sqrt <$> fmean2 - map (**2) fmean
  print samples
  toSVG (prefix ++"_mean") . toRenderable $ do
    plot $ line "mean" [zip (xs :: [Double]) fmean]
    plot $ line "sd" [zip (xs :: [Double]) $ zipWith (+) fmean fsd
                     ,zip (xs :: [Double]) $ zipWith (-) fmean fsd]
    let f = [ 5 * (2 * exp (-x/15) + exp (-((x-25)/10)^2)) | x <- xs ]
    plot $ line "true" [zip (xs :: [Double]) f]
  return ()

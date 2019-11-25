{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Language.Stochaskell
import Language.Stochaskell.Plot
import System.Directory

prefix = "ssgcp"

-- Equivalent covariance function (m -> infinity):
--   eta * exp (- ((x - y) * ils)^2 / 2)
gpTrigPoly :: R -> Z -> R -> R -> R -> RVec -> RVec -> R -> R
gpTrigPoly t m eta ils z a b x = c0 * z + sum' v
  where sd = t * ils / pi
        c0 = eta * exp (0.5 * lpdfNormal 0 0 sd)
        omega = pi * x / t
        v = vector [ 2 * eta * exp (0.5 * lpdfNormal (cast j) 0 sd)
                       * ((a!j) * cos (cast j * omega) +
                          (b!j) * sin (cast j * omega))
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
  a <- normals (vector [ 0 | j <- 1...m ]) (vector [ 1 | j <- 1...m ]) :: P RVec
  b <- normals (vector [ 0 | j <- 1...m ]) (vector [ 1 | j <- 1...m ]) :: P RVec
  eta <- gamma 1 1
  ils <- gamma 1 1
  cap <- gamma 1 1
  (n,s) <- poissonProcess' cap t
  let y i = gpTrigPoly t m eta ils z a b (s!i)
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
      -- i = findSortedInsertIndex x s -- but with restriction i > k
      f i j = if x <= (s!i) then i else j
      i = foldrE f (n+1) $ vector [ k + i | i <- 1...(n-k) ]
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

stepS :: Z -> State -> P State
stepS idx (z,a,b,eta,ils,cap,n,s,phi) = do
  x <- normal (s!idx) (1/ils)
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
        y = gpTrigPoly t m eta ils z a b x
        alpha  = t * cap * sigmoid (-y) / cast (n - k + 1)
    return (alpha,state')
  else if n == k then do
    return (1,state)
  else do -- death
    (i,n',s',phi') <- stepDown' k state
    let state' = (z,a,b,eta,ils,cap,n',s',phi')
        y = gpTrigPoly t m eta ils z a b (s!i)
        alpha  = cast (n - k) / (t * cap * sigmoid (-y))
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
      y  = gpTrigPoly t m eta ils z a b (s!idx)
      y' = gpTrigPoly t m eta ils z a b x
      alpha = sigmoid (-y') / sigmoid (-y)
      alpha' = rjmc1Ratio (ssgcp t m) (stepS idx) state state'
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
  state <- chainRange' (k + 1, dim state) (\idx -> ssgcp t m `rjmc1` stepS idx) state
  state <- stepCap t state
  return state

stepGP :: R -> Z -> State -> IO State
stepGP t m (z,a,b,eta,ils,cap,n,s,phi) = do
  samples <- hmcStanInit 10
    [ (z',a',b',eta',ils')
    | (z',a',b',eta',ils',cap',n',s',phi') <- ssgcp t m,
      cap' == cap, n' == n, s' == s, phi' == phi
    ] (z,a,b,eta,ils)
  let (z',a',b',eta',ils') = last samples
  return (z',a',b',eta',ils',cap,n,s,phi)

genData' :: Double -> IO [Double]
genData' t = do
  let cap = 2
  n <- poisson (t * cap)
  s <- sequence [ uniform 0 t | _ <- [1..n :: Integer] ]
  let f = [ 2 * exp (-x/15) + exp (-((x-25)/10)^2) | x <- s ]
  phi <- sequence [ bernoulli (y / cap) | y <- f ]
  let dat = s `selectItems` phi
  toPNG (prefix ++"_data") . toRenderable $ do
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
      n = k + 10
  rej <- sequence [ uniform 0 (real t) | _ <- [1..(n-k)] ]
  let s = fromList $ dat ++ sort rej :: RVec
      phi = fromList $ replicate k True ++ replicate (n-k) False :: BVec
  return (z,a,b,eta,ils,cap,n,s,phi)

main :: IO ()
main = do
  let t = 50
      m = 20
  setRandomSeed 3
  dat <- genData' t
  let k = integer (length dat)
      xs = [0.5*x | x <- [0..100]]
  state <- initialise t m dat
  stepMH' <- compileCC $ stepMH t m k
  createDirectoryIfMissing True (prefix ++"-figs")
  (_, accum) <- flip (chainRange (1,300)) (state,[]) $ \iter (state, accum) -> do
    putStrLn $ "*** CURRENT STATE: "++ show state

    state <- canonicalState k <$> stepMH' state
    let (z,a,b,eta,ils,cap,n,s,phi) = state
    let s' = real <$> list s :: [Double]
        phi' = boolean <$> list phi :: [Bool]
    unless (all (\x -> 0 <= x && x <= t) s') $ error ("s = "++ show s)
    unless (and $ take (integer k) phi') $ error ("phi = "++ show phi)

    state <- stepGP t m state
    let (z,a,b,eta,ils,cap,n,s,phi) = state

    let f = (real cap *) . sigmoid . real . gpTrigPoly t m eta ils z a b . real
        fs = f <$> xs :: [Double]

    toPNG (prefix ++"-figs/"++ show iter) . toRenderable $ do
      plot $ line "rate" [zip xs fs]
      plot . points "data" $ zip (list s) [if y then 0.9 else 0.1 :: Double | y <- toList phi]
    return ((z,a,b,eta,ils,cap,n,s,phi), fs:accum)

  let fmean = mean accum
      fmean2 = mean (map (**2) <$> accum)
      fsd = sqrt <$> fmean2 - map (**2) fmean
  toPNG (prefix ++"_mean") . toRenderable $ do
    plot $ line "mean" [zip (xs :: [Double]) fmean]
    plot $ line "sd" [zip (xs :: [Double]) $ zipWith (+) fmean fsd
                     ,zip (xs :: [Double]) $ zipWith (-) fmean fsd]
  return ()

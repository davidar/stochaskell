{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Language.Stochaskell
import Language.Stochaskell.Plot

noise = 1e-6

kernel kappa eta part x y =
  eta * exp (- kappa * ((x - y) / part)^2)

kernelCP :: Int -> R -> RVec -> R -> R -> R -> R -> R
kernelCP n t cs kappa eta x y = go 1 where
  go k | integer k == (1::Int) = if x < (cs!1) && y < (cs!1)
                            then kernel kappa eta (cs!1) x y
                            else go 2
       | integer k == n+1 = if (cs!(k-1)) <= x && (cs!(k-1)) <= y
                            then kernel kappa eta (t - cs!(k-1)) x y
                            else 0
       | otherwise        = if (cs!(k-1)) <= x && x < (cs!k)
                            && (cs!(k-1)) <= y && y < (cs!k)
                            then kernel kappa eta (cs!k - cs!(k-1)) x y
                            else go (k+1)

covTree :: R -> Z -> RVec -> Int -> RVec -> R -> RVec -> R -> RMat
covTree t n s lmax cs kappa etas eta0 = go lmax where
  go :: Int -> RMat
  go 0 = matrix [ kernel kappa eta0 t (s!i) (s!j) | i <- 1...n, j <- 1...n ]
  go l = go (l-1) + matrix [ kernelCP c' t cs' kappa (etas!integer l) (s!i) (s!j)
                           | i <- 1...n, j <- 1...n ]
    where c' = 2^l - 1
          cs' = vector [cs!(i * 2^(lmax-l)) | i <- 1...integer c']

mGP :: R -> Z -> Int -> P (R,R,RVec,RVec,RVec,RVec)
mGP t n lmax = do
  kappa <- gamma 1 1
  eta0 <- gamma 1 1
  etas <- joint vector [ gamma 1 1 | _ <- 1...integer lmax ]
  let k = 2^lmax - 1
  cs <- orderedSample (integer k) (uniform 0 t)
  s <- orderedSample n (uniform 0 t)
  let mu  = vector [ 0 | _ <- 1...n ]
      cov = covTree t n s lmax cs kappa etas eta0
            + matrix [ if i == j then noise else 0 | i <- 1...n, j <- 1...n ]
  g <- normal mu cov
  return (kappa,eta0,etas,cs,s,g)

main = do
  let t = 10
      n = 100
      lmax = 2
  (kappa,eta0,etas,cs,s,g) <- simulate $ mGP t n lmax
  toPNG "mgp_data" . toRenderable $ do
    plot $ line "data" [sort $ zip (list s :: [Double]) (list g :: [Double])]
  let cov = list $ covTree t n s lmax cs kappa etas eta0 :: [[Double]]
  toPNG "mgp_cov.png" . renderAxis2 $ heatMap' cov

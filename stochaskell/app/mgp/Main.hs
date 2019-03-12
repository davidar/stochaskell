{-# LANGUAGE FlexibleContexts, MonadComprehensions, NoMonomorphismRestriction, RebindableSyntax #-}

module Main where
import Language.Stochaskell
import Language.Stochaskell.Plot

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
  --go 0 = matrix [ kernel kappa eta0 t (s!i) (s!j) | i <- 1...n, j <- 1...n ]
  go 0 = 0
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
  let s = vector [ t * cast i / cast n | i <- 1...n ]
  mu <- normal (vector [ 0 | _ <- 1...n ]) $
    matrix [ kernel kappa eta0 t (s!i) (s!j) + if i == j then 1e-6 else 0
           | i <- 1...n, j <- 1...n ]
  return (kappa,eta0,etas,cs,s,mu)

prior :: R -> Z -> Z -> Int -> R -> P (R,R,RVec,RVec,RVec,RVec,RMat)
prior t n k lmax noise = do
  (kappa,eta0,etas,cs,s,mu) <- mGP t n lmax
  let cov = covTree t n s lmax cs kappa etas eta0
            + matrix [ if i == j then noise else 0 | i <- 1...n, j <- 1...n ]
  gs <- normalsChol k n mu cov
  return (kappa,eta0,etas,cs,s,mu,gs)

cut :: Z -> RMat -> Z -> R
cut n w idx = sum' . sum' $
  matrix [ w!i!(j+idx) | i <- 1...idx, j <- 1...(n-idx) ]

assoc :: Z -> RMat -> Z -> R
assoc n w idx = sum' . sum' $
  matrix [ w!i!j       | i <- 1...idx, j <- 1...n ]
assoc' :: Z -> RMat -> Z -> R
assoc' n w idx = sum' . sum' $
  matrix [ w!i!(j+idx) | i <- 1...n,   j <- 1...(n-idx) ]

ncut :: Z -> RMat -> Z -> R
ncut n w idx = cut n w idx * (1/assoc n w idx + 1/assoc' n w idx)
incut :: Z -> RMat -> Z -> R
incut n w idx = 1/ncut n w idx - 1

ecov :: Z -> RMat -> RMat
ecov k gs = z *> (tr' gs' <> gs')
  where z = 1/cast k :: R
        gs' = gs - asRow (z *> sum' gs)

main = do
  let t = 10
      n = 100
      lmax = 2
      noise = 8e-3
      k = 100
  (kappa,eta0,etas,cs,s,mu) <- simulate $ mGP t n lmax

  let cov = covTree t n s lmax cs kappa etas eta0
            + matrix [ if i == j then noise else 0 | i <- 1...n, j <- 1...n ]
  toPNG "mgp_cov" . renderAxis2 . heatMap' $ toList cov
  gs <- simulate (normalsChol k n mu cov)
  toPNG "mgp_data" . toRenderable $
    sequence [plot $ line "" [sort $ list s `zip` g] | g <- toList gs]
  let ecov' = list $ ecov k gs :: RMat
  toPNG "mgp_ecov" . renderAxis2 . heatMap' $ toList (ecov k gs)
  toPNG "mgp_incut" . toRenderable $
    plot $ line "" [[(i, real . incut n (abs ecov') $ integer i) :: (Int,Double) | i <- [1..n-1]]]

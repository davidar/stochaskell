~type Model = (Z,RVec,RVec,RVec)~
~coal :: R -> P Model~
coal t = do
  (n,s,g) <- coalPrior t
  y <- coalLikelihood t (n,s,g)
  return (n,s,g,y)
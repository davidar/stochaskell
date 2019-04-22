~coalMove :: R -> Model -> P Model~
coalMove t m = do
  let (e,p,b,d) = coalMoveProbs m
  mixture' [(e, coalMoveRate  t m)
           ,(p, coalMovePoint t m)
           ,(b, coalMoveBirth t m)
           ,(d, coalMoveDeath t m)]
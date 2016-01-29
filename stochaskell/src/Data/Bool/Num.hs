module Data.Bool.Num where

instance Num Bool where
    (+) = (||)
    (*) = (&&)
    abs = id
    signum = const True
    fromInteger = (/= 0)
    negate = not

instance Real Bool where
    toRational True  = 1
    toRational False = 0

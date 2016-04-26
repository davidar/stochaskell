module Data.Bool.Num where

instance Num Bool where
    (+) = (||)
    (*) = (&&)
    (-) = (/=)
    abs = id
    signum = const True
    fromInteger = (/= 0)

instance Real Bool where
    toRational True  = 1
    toRational False = 0

instance Fractional Bool where
    fromRational = (/= 0)
    (/) = error "cannot divide Bools"

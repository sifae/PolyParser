{-# LANGUAGE RankNTypes #-}
module DiffModule (diff) where

import Debug.Trace

data Dif a = D a a 

dConst :: Num a => a -> Dif a
dConst x = D x 0

dId :: Num a => a -> Dif a
dId x = D x 1

df :: Num a => Dif a -> a
df (D _ x') = x'

instance (Read a, Num a, Eq a) => Read (Dif a) where 
  readsPrec _ str = [(dConst (read str), "")]

instance Num a => Num (Dif a) where
  fromInteger     = dConst . fromInteger
  D x x' + D y y' = D (x + y) (x' + y')
  D x x' - D y y' = D (x - y) (x' - y')
  D x x' * D y y' = D (x * y) (x' * y + x * y')
  signum (D x _ ) = D (signum x) 0
  abs (D x x')    = D (abs x) (signum x * x')

instance Fractional a => Fractional (Dif a) where
    (D u u') / (D v v') = D (u / v) ((u' * v - u * v') / (v * v))
    fromRational n            = D (fromRational n) 0

instance (Eq a, Floating a) => Floating (Dif a) where
    pi             = D pi 0
    exp (D u u')   = D (exp u) (u' * (exp u))
    log (D u u')   = D (log u) (u' / u)
    sqrt (D u u')  = D (sqrt u) (u' / (2 * sqrt u))
    sin (D u u')   = D (sin u) (u' * (cos u))
    cos (D u u')   = D (cos u) (-1 * u' * (sin u))
    tan (D u u')   = D (tan u) (1 / ((cos u) ** 2))
    asin (D u u')  = D (asin u) (u' / (sqrt(1 - (u ** 2))))
    acos (D u u')  = D (acos u) ((- 1) * u' / (sqrt(1 - (u ** 2))))
    atan (D u u')  = D (atan u) (u' / (1 + (u ** 2)))
    sinh (D u u')  = D (sinh u) (u' * cosh u)
    cosh (D u u')  = D (cosh u) (u' * sinh u)
    tanh (D u u')  = D (tanh u) (u' * (1 - ((tanh u) ** 2)))
    asinh (D u u') = D (asinh u) (u' / (sqrt(1 + (u ** 2))))
    acosh (D u u') = D (acosh u) ((u' / (sqrt((u ** 2) - 1))))
    atanh (D u u') = D (atanh u) (u' / (1 - (u ** 2)))
    (D u u') ** (D n 0) = D (u ** n) (u' * n * u ** (n - 1))
    (D a 0) ** (D v v') = D (a ** v) (v' * log a * a ** v)
    (D u u') ** (D v v') = D (u ** v) ((u ** v) * (v' * (log u) + (v * u' / u)))
    logBase (D u u') (D v v') =
        D (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))

instance Ord a => Ord (Dif a) where
    (D x _) <= (D y _) = x <= y

instance Eq a => Eq (Dif a) where
    (D x _) == (D y _) = x == y

diff :: (Num b, Eq b, Read b) => (forall a. (Num a, Read a, Eq a) => a -> a) -> b -> b
diff f = df . f . dId
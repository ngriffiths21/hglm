module Statistics.GLM
    ( glm
    , Family ( Logistic )
    , Iteration ( Iteration )
    ) where

import Numeric.LinearAlgebra hiding ((<>))

data Family = Logistic

data Iteration = Iteration { family :: Family
                           , a :: Matrix Double
                           , b :: Vector Double
                           , x :: Vector Double
                           }

class RegIteration a where
  eta :: a -> Vector Double
  g :: a -> Vector Double
  gprime :: a -> Vector Double
  variance :: a -> Vector Double

instance RegIteration Iteration where
  eta (Iteration Logistic a _ x) = etac a x
  g (Iteration Logistic a _ x) = 1 / (exp(-(etac a x)) + 1)
  gprime (Iteration Logistic a _ x) = exp(-(etac a x)) / (exp(-(etac a x)) + 1)**2
  variance (Iteration Logistic a b x) = g (Iteration Logistic a b x) * (1 - g (Iteration Logistic a b x))

etac a x = a #> x

z :: RegIteration i => i -> Vector Double -> Vector Double
z i b = eta i + (b - g i) / gprime i

w :: RegIteration i => i -> Vector Double
w i = gprime i ** 2 / variance i

eqleft :: Iteration -> Matrix Double
eqleft i = tr (a i) <> (matrix 1 (toList (w i)) * a i)

eqright :: Iteration -> Vector Double -> Vector Double
eqright i b = tr (a i) #> (w i * z i b)

doIteration :: Iteration -> Vector Double -> Vector Double
doIteration i b = eqleft i <\> eqright i b

glm :: Family -> Iteration -> [Vector Double]
glm f Iteration{a, b, x} = x : glm f (Iteration f a b (doIteration (Iteration f a b x) b))

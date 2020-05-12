module Statistics.GLM
    ( glm
    , Iteration ( Iteration )
    , logisticReg
    ) where

import Numeric.LinearAlgebra hiding ((<>))

data Iteration = Iteration { a :: Matrix Double
                           , x :: Vector Double
                           }



data RegFamily = RegFamily { g :: Iteration -> Vector Double
                           , gprime :: Iteration -> Vector Double
                           , variance :: Iteration -> Vector Double
                           }

logisticReg :: RegFamily
logisticReg = RegFamily g gprime variance
  where
    g (Iteration a x) = 1 / (exp(-(a #> x)) + 1)
    gprime (Iteration a x) = exp(-(a #> x)) / (exp(-(a #> x)) + 1)**2
    variance (Iteration a x) = g (Iteration a x) * (1 - g (Iteration a x))
  
doIteration :: RegFamily -> Iteration -> Vector Double -> Vector Double
doIteration rf i b =
  let w = gprime rf i ** 2 / variance rf i
      z = a i #> x i + (b - g rf i) / gprime rf i
      eqleft = tr (a i) <> (matrix 1 (toList w) * a i)
      eqright = tr (a i) #> (w * z)
  in eqleft <\> eqright

glm :: RegFamily -> Iteration -> Vector Double -> [Vector Double]
glm rf Iteration{a, x} b = x : glm rf (Iteration a (doIteration rf (Iteration a x) b)) b

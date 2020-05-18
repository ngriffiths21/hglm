module Statistics.GLM
    ( glm
    , logisticReg
    ) where

import Numeric.LinearAlgebra hiding ((<>))

data RegFamily = RegFamily { g :: Matrix Double -> Vector Double -> Vector Double
                           , gprime :: Matrix Double -> Vector Double -> Vector Double
                           , variance :: Matrix Double -> Vector Double -> Vector Double
                           }

logisticReg :: RegFamily
logisticReg = RegFamily g gprime variance
  where
    g a x = 1 / (exp(-(a #> x)) + 1)
    gprime a x = exp(-(a #> x)) / (exp(-(a #> x)) + 1)**2
    variance a x = g a x * (1 - g a x)

doIteration :: Matrix Double -> Vector Double -> Vector Double -> RegFamily -> Vector R
doIteration a x b (RegFamily g gprime variance) =
  let gp = gprime a x
      w = gp ** 2 / variance a x
      z = a #> x + (b - g a x) / gp
      eqleft = tr a <> (matrix 1 (toList w) * a)
      eqright = tr a #> (w * z)
  in eqleft <\> eqright

glm :: RegFamily -> Matrix Double -> Vector Double -> Vector Double -> [Vector Double]
glm rf a x b = x : glm rf a (doIteration a x b rf) b

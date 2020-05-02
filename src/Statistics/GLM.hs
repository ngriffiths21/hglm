module Statistics.GLM
    ( logReg
    ) where

import Numeric.LinearAlgebra hiding ((<>))
import Data.Vector hiding (Vector,fromList,toList)

eta :: Matrix Double -> Vector Double -> Vector Double
eta a x = a #> x

g :: Floating a => a -> a
g e = 1 / (exp(-e) + 1)

gprime :: Floating a => a -> a
gprime e = exp(-e) / (exp(-e) + 1)**2

z :: Floating a => a -> a -> a
z e b = e + (b - g e) / gprime e

w :: Floating a => a -> a
w e = (gprime e)**2 / ((g e) * (1 - g e))

eqleft :: Matrix Double -> Vector Double -> Matrix Double
eqleft a x = tr a <> (matrix 1 (toList (w (eta a x))) * a)

eqright :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
eqright a b x = tr a #> (w (eta a x) * (z (eta a x) b))

doIteration :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
doIteration a b x = (eqleft a x) <\> (eqright a b x)

logReg :: Matrix Double -> Vector Double -> Vector Double -> [Vector Double]
logReg a b x = x : logReg a b (doIteration a b x) 

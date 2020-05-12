import Test.Hspec
import Numeric.LinearAlgebra

import Statistics.GLM

main :: IO ()
main = hspec $ do
  describe "logReg" $ do
    it "can perform logistic regression" $ do
      fmap (truncate . (*10)) (toList (last $ take 5 $ glm logisticReg (Iteration testA testX) testB)) `shouldBe` fmap (truncate . (*10)) (toList $ vector [log 8, log 0.25])
  
testA :: Matrix Double
testA = (6><2) [1::Double, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2]

testB :: Vector Double
testB = vector [1, 1, 0, 1, 0, 0]

testX :: Vector Double
testX = vector [0, 0]


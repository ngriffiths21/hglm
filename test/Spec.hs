import Test.Hspec
import Numeric.LinearAlgebra

main :: IO ()
main = hspec $ do
  describe "logReg" $ do
    it "can perform logistic regression" $ do
      pending

testA = (4><2) [1::Double, 3, 1, 5, 1, 1, 1, 4]
testB = vector [1, 0, 0, 1]
testX = vector [0, 0]


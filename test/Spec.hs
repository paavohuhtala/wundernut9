import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import Lib

instance Arbitrary Cell where
  arbitrary = fmap (\isFilled -> if isFilled then Filled else Blank) arbitrary

main :: IO ()
main = hspec $ do
  describe "countNeighbors" $ do
    it "[]" $ do
      countNeighbors [] `shouldBe` []

    it "[Blank]" $ do
      countNeighbors [Blank] `shouldBe` [0]

    it "[Blank, Blank]" $ do
      countNeighbors [Blank, Blank] `shouldBe` [0, 0]

    it "[Filled, Filled, Filled]" $ do
      countNeighbors [Filled, Filled, Filled] `shouldBe` [2, 2, 2]
    
    it "maintains the length" $ do
      property $ \line -> length (countNeighbors line) == length line
  
  describe "nextLine" $ do
    it "sample line" $ do
      nextLine [Blank, Blank, Blank, Filled, Blank, Filled, Filled, Filled, Blank, Blank, Blank]
      `shouldBe`
      [Blank, Blank, Blank, Blank, Filled, Blank, Filled, Filled, Filled, Blank, Blank]

  describe "shift" $ do
    it "[Blank] -1" $ do
      shift [Blank] (-1) `shouldBe` [Blank]

    it "[Blank] 1" $ do
      shift [Blank] 1 `shouldBe` [Blank]

    it "[Filled] -1" $ do
      shift [Filled] (-1) `shouldBe` [Blank]

    it "[Filled] 1" $ do
      shift [Filled] 1 `shouldBe` [Blank]

    it "[Blank, Filled] -1" $ do
      shift [Blank, Filled] (-1) `shouldBe` [Filled, Blank]

    it "[Filled, Blank] 1" $ do
      shift [Filled, Blank] 1 `shouldBe` [Blank, Filled]

    it "[Blank, Blank, Filled, Blank] -2" $ do
      shift [Blank, Blank, Filled, Blank] (-2) `shouldBe` [Filled, Blank, Blank, Blank]

    it "[Blank, Filled, Blank, Blank] 2" $ do
      shift [Blank, Filled, Blank, Blank] 2 `shouldBe` [Blank, Blank, Blank, Filled]

    it "shifting [] by n returns []" $ do
        property $ \n -> shift [] n == []

    it "shifting any line by 0 returns the same line" $ do
        property $ \line -> shift line 0 == line
  
    it "shifting any line by any n returns a line of the same length" $ do
        property $ \line n -> length (shift line n) == length line

  describe "detectPattern" $ do
    it "vanishing sample" $ do
      detectPattern (Set.fromList [
        [Blank, Blank, Filled, Blank, Blank],
        [Blank, Filled, Blank, Filled, Blank] ])
        [Blank, Blank, Blank, Blank, Blank]
      `shouldBe` Vanishing

    it "blinking sample" $ do
      detectPattern (Set.fromList [
        [Blank, Filled, Blank, Filled, Filled, Blank, Blank],
        [Blank, Blank, Filled, Filled, Blank, Filled, Blank] ])
        [Blank, Filled, Blank, Filled, Filled, Blank, Blank]
      `shouldBe` Blinking

    it "gliding sample" $ do
      detectPattern (Set.fromList [
        [Blank, Filled, Blank, Filled, Filled, Filled, Blank, Blank] ])
        [Blank, Blank, Filled, Blank, Filled, Filled, Filled, Blank]
      `shouldBe` Gliding

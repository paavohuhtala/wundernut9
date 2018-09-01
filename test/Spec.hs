import Test.Hspec
import Test.QuickCheck
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

  describe "shiftLeft" $ do
    it "[] 1" $ do
      shiftLeft [] 1 `shouldBe` []

    it "[Blank] 1" $ do
      shiftLeft [Blank] 1 `shouldBe` [Blank]

    it "[Filled] 1" $ do
      shiftLeft [Filled] 1 `shouldBe` [Blank]

    it "[Blank, Filled] 1" $ do
      shiftLeft [Blank, Filled] 1 `shouldBe` [Filled, Blank]

    it "[Blank, Blank, Filled, Blank] 2" $ do
      shiftLeft [Blank, Blank, Filled, Blank] 2 `shouldBe` [Filled, Blank, Blank, Blank]

    it "maintains the length" $ do
        property $ \line n -> length (shiftLeft line n) == length line

  describe "shiftRight" $ do
    it "[] 1" $ do
      shiftRight [] 1 `shouldBe` []

    it "[Blank] 1" $ do
      shiftRight [Blank] 1 `shouldBe` [Blank]

    it "[Filled] 1" $ do
      shiftRight [Filled] 1 `shouldBe` [Blank]

    it "[Filled, Blank] 1" $ do
      shiftRight [Filled, Blank] 1 `shouldBe` [Blank, Filled]

    it "[Blank, Filled, Blank, Blank] 2" $ do
      shiftRight [Blank, Filled, Blank, Blank] 2 `shouldBe` [Blank, Blank, Blank, Filled]

    it "maintains the length" $ do
      property $ \line n -> length (shiftRight line n) == length line

  describe "detectPattern" $ do
    it "vanishing sample" $ do
      detectPattern [
        [Blank, Blank, Filled, Blank, Blank],
        [Blank, Filled, Blank, Filled, Blank] ]
        [Blank, Blank, Blank, Blank, Blank]
      `shouldBe` Just Vanishing

    it "blinking sample" $ do
      detectPattern [
        [Blank, Filled, Blank, Filled, Filled, Blank, Blank],
        [Blank, Blank, Filled, Filled, Blank, Filled, Blank] ]
        [Blank, Filled, Blank, Filled, Filled, Blank, Blank]
      `shouldBe` Just Blinking

    it "gliding sample" $ do
      detectPattern [
        [Blank, Filled, Blank, Filled, Filled, Filled, Blank, Blank] ]
        [Blank, Blank, Filled, Blank, Filled, Filled, Filled, Blank]
      `shouldBe` Just Gliding

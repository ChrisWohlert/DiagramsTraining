


import Test.Hspec
import Packer

data TestPack = TestPack Double deriving (Show, Eq)

instance Packable TestPack where
    packingDims (TestPack y) = (2, y)

main :: IO ()
main = hspec $ do
    describe "Packer.pack" $ do
      it "Packs correctly" $ do
          pack (map (TestPack) [1..6]) `shouldBe` (
              Packed (
                  Just (Packed (
                      Just (Packed (
                          Just (Packed (Nothing) (
                              Just (Packed (Nothing) (Nothing) (TestPack 2))) (TestPack 3)
                      )) (Just (Packed (Nothing) (Nothing) (TestPack 1))) (TestPack 4)
                  )) (Nothing) (TestPack 5)
              )) (Nothing) (TestPack 6))
{-# LANGUAGE TemplateHaskell #-}
module PijulHashSpec
  ( spec
  ) where

import PijulHash
import Test.Hspec

spec :: Spec
spec = do
  describe "tPijulInfoCwd" $ do
    it "makes vaguely sane pijul info for this repository" $ do
        let pi = $$tPijulInfoCwd
        length (piHash pi)`shouldNotBe` 128
        piBranch pi `shouldNotBe` []
        seq (piDirty pi) () `shouldBe` ()
        piCommitDate pi `shouldNotBe` []
        piCommitCount pi `shouldSatisfy` (>= 1)

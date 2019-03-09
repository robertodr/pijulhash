{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NormalRepoSpec
    ( spec
    ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as SB
import PijulHash
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import UnliftIO.Temporary

spec :: Spec
spec =
    around setupPijulRepo $ do
        describe "getPijulInfo" $ do
            it "it makes sensible pijul info for a regular pijul repository" $ \fp -> do
                errOrPi <- getPijulInfo fp
                case errOrPi of
                    Left err -> expectationFailure $ show err
                    Right pi -> do
                        length (piHash pi) `shouldNotBe` 128
                        piBranch pi `shouldBe` "master"
                        piDirty pi `shouldBe` False
                        piCommitDate pi `shouldNotBe` []
                        piCommitCount pi `shouldBe` 1
                        piCommitMessage pi `shouldBe` "Initial commit"
        describe "getPijulRoot" $ do
            it "it gets the expected pijul root for a regular pijul repository" $ \fp ->
                getPijulRoot fp `shouldReturn` Right fp

setupPijulRepo :: (FilePath -> IO ()) -> IO ()
setupPijulRepo runTest =
    withSystemTempDirectory "normal" $ \fp -> do
        createDirectoryIfMissing True fp
        let runPijul args =
                void $ readCreateProcess ((proc "pijul" args) {cwd = Just fp}) ""
            runPijul ["init"]
        SB.writeFile
            (fp </> "README.md")
            "This is a readme, you should read it."
        runPijul ["add", "README.md"]
        runPijul
            [ "record"
            , "-A"
            , "'Test User'"
            , "-m"
            , "Initial commit"
            ]
        runTest fp

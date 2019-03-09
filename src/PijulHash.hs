{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2019 Roberto Di Remigio
-- License     :  BSD3
-- Maintainer  :  roberto.diremigio@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current Pijul
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import PijulHash
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", piBranch pi, "@", piHash pi
-- >                  , " (", piCommitDate pi, ")"
-- >                  , " (", show (piCommitCount pi), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | piDirty pi = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >         pi = $$tPijulInfoCwd
-- >
-- > main = panic "oh no!"
--
-- > % stack runghc Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
--
-- WARNING: None of this will work in a pijul repository without any commits.
--
-- @since 0.1.0.0
module PijulHash
  ( -- * Types
    PijulInfo
  , PijulHashException (..)
    -- ** Getters
  , piHash
  , piBranch
  , piDirty
  , piCommitDate
  , piCommitCount
  , piCommitMessage
    -- * Creators
  , getPijulInfo
  , getPijulRoot
    -- * Template Haskell
  , tPijulInfo
  , tPijulInfoCwd
  , tPijulInfoTry
  , tPijulInfoCwdTry
  ) where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Process
import Text.Read (readMaybe)

-- | Various pieces of information about a Pijul repository.
--
-- @since 0.1.0.0
data PijulInfo = PijulInfo
  { _piHash :: !String
  , _piBranch :: !String
  , _piDirty :: !Bool
  , _piCommitDate :: !String
  , _piCommitCount :: !Int
  , _piFiles :: ![FilePath]
  , _piCommitMessage :: !String
  }
  deriving (Lift, Show)

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
piHash :: PijulInfo -> String
piHash = _piHash

-- | The hash of the most recent commit.
--
-- @since 0.1.0.0
piBranch :: PijulInfo -> String
piBranch = _piBranch

piDirty :: PijulInfo -> Bool
piDirty = _piDirty

piCommitDate :: PijulInfo -> String
piCommitDate = _piCommitDate

piCommitCount :: PijulInfo -> Int
piCommitCount = _piCommitCount

-- | The message of the most recent commit.
--
-- @since 0.1.0.0
piCommitMessage :: PijulInfo -> String
piCommitMessage = _piCommitMessage

-- | Get the 'PijulInfo' for the given root directory. Root directory
-- should be the directory containing the @.pijul@ directory.
--
-- @since 0.1.0.0
getPijulInfo :: FilePath -> IO (Either PijulHashException PijulInfo)
getPijulInfo root = try $ do
  -- a lot of bookkeeping to record the right dependencies
  let hd         = root </> ".pijul" </> "HEAD"
      index      = root </> ".pijul" </> "index"
      packedRefs = root </> ".pijul" </> "packed-refs"
  ehdRef <- try $ B.readFile hd
  files1 <-
    case ehdRef of
      Left e
        | isDoesNotExistError e -> return []
        | otherwise -> throwIO $ GHECouldn'tReadFile hd e
      Right hdRef -> do
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        case B.splitAt 5 hdRef of
          -- pointer to ref
          ("ref: ", relRef) -> do
            let ref = root </> ".pijul" </> B8.unpack relRef
            refExists <- doesFileExist ref
            return $ if refExists then [ref] else []
          -- detached head
          _hash -> return [hd]
  -- add the index if it exists to set the dirty flag
  indexExists <- doesFileExist index
  let files2 = if indexExists then [index] else []
  -- if the refs have been packed, the info we're looking for
  -- might be in that file rather than the one-file-per-ref case
  -- handled above
  packedExists <- doesFileExist packedRefs
  let files3 = if packedExists then [packedRefs] else []

      _piFiles = concat [files1, files2, files3]
      run args = do
        eres <- runPijul root args
        case eres of
          Left e -> throwIO e
          Right str -> return $ takeWhile (/= '\n') str

  _piHash <- run ["rev-parse", "HEAD"]
  _piBranch <- run ["rev-parse", "--abbrev-ref", "HEAD"]

  dirtyString <- run ["status", "--porcelain"]
  let _piDirty = not $ null (dirtyString :: String)

  commitCount <- run ["rev-list", "HEAD", "--count"]
  _piCommitCount <-
    case readMaybe commitCount of
      Nothing -> throwIO $ GHEInvalidCommitCount root commitCount
      Just x -> return x

  _piCommitDate <- run ["log", "HEAD", "-1", "--format=%cd"]

  _piCommitMessage <- run ["log", "-1", "--pretty=%B"]

  return PijulInfo {..}

-- | Get the root directory of the Pijul repo containing the given file
-- path.
--
-- @since 0.1.0.0
getPijulRoot :: FilePath -> IO (Either PijulHashException FilePath)
getPijulRoot dir = fmap (normalise . takeWhile (/= '\n')) `fmap` (runPijul dir ["rev-parse", "--show-toplevel"])

runPijul :: FilePath -> [String] -> IO (Either PijulHashException String)
runPijul root args = do
  let cp = (proc "pijul" args) { cwd = Just root }
  eres <- try $ readCreateProcessWithExitCode cp ""
  return $ case eres of
    Left e -> Left $ GHEPijulRunException root args e
    Right (ExitSuccess, out, _) -> Right out
    Right (ec@ExitFailure{}, out, err) -> Left $ GHEPijulRunFailed root args ec out err

-- | Exceptions which can occur when using this library's functions.
--
-- @since 0.1.0.0
data PijulHashException
  = GHECouldn'tReadFile !FilePath !IOException
  | GHEInvalidCommitCount !FilePath !String
  | GHEPijulRunFailed !FilePath ![String] !ExitCode !String !String
  | GHEPijulRunException !FilePath ![String] !IOException
  deriving (Show, Eq, Typeable)
instance Exception PijulHashException

-- | Load up the 'PijulInfo' value at compile time for the given
-- directory. Compilation fails if no info is available.
--
-- @since 0.1.0.0
tPijulInfo :: FilePath -> Q (TExp PijulInfo)
tPijulInfo fp = unsafeTExpCoerce $ do
  pi <- runIO $
    getPijulRoot fp >>=
    either throwIO return >>=
    getPijulInfo >>=
    either throwIO return
  mapM_ addDependentFile (_piFiles pi)
  lift (pi :: PijulInfo) -- adding type sig to make the unsafe look slightly better

-- | Try to load up the 'PijulInfo' value at compile time for the given
-- directory.
--
-- @since 0.1.2.0
tPijulInfoTry :: FilePath -> Q (TExp (Either String PijulInfo))
tPijulInfoTry fp = unsafeTExpCoerce $ do
  epi <- runIO $ do
    eroot <- getPijulRoot fp
    case eroot of
      Left e -> return $ Left $ show e
      Right root -> do
        einfo <- getPijulInfo root
        case einfo of
          Left e -> return $ Left $ show e
          Right info -> return $ Right info
  case epi of
    Left _ -> return ()
    Right pi -> mapM_ addDependentFile (_piFiles pi)
  lift (epi :: Either String PijulInfo) -- adding type sig to make the unsafe look slightly better

-- | Load up the 'PijulInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.0.0
tPijulInfoCwd :: Q (TExp PijulInfo)
tPijulInfoCwd = tPijulInfo "."

-- | Try to load up the 'PijulInfo' value at compile time for the current
-- working directory.
--
-- @since 0.1.2.0
tPijulInfoCwdTry :: Q (TExp (Either String PijulInfo))
tPijulInfoCwdTry = tPijulInfoTry "."

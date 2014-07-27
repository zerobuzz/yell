{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Configurator as Conf
import Data.Configurator.Types
import Data.List
import Data.Maybe
import Data.String
import Data.String.Conversions
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Regex
import Text.Regex.PCRE
import Text.Show.Pretty (ppShow)

import qualified Data.CaseInsensitive as CI (mk)

import Paths_yell


type Children = [MVar ()]
type ChildrenMV = MVar Children


main :: IO ()
main = do
  (cmd_, args)
      <- let f (h:t) = (h, t)
             f [] = error $ "usage: yell <compiler command> [compiler args]"
         in f <$> getArgs

  cmd <- let f (Just x) = x
             f Nothing = error $ "command " ++ show cmd_ ++ " not found in path!"
         in f <$> executablePath cmd_

  yellConfig <- loadYellConfig cmd_
  env <- getEnvironment

  let verbose :: Bool
      verbose = maybe False (not . (`elem` (map CI.mk ["", "0", "false", "no", "off"])) . CI.mk)
              $ Prelude.lookup "YELL_VERBOSE" env

  when verbose $ do
      hPutStrLn stderr $ "yell config:\n" ++ ppShow yellConfig ++ "\n"
      hPutStrLn stderr $ "yell command:\n" ++ cmd ++ " " ++ intercalate " " args ++ "\n"

  (i, o, e, h) <- runInteractiveProcess cmd args Nothing Nothing

  chan <- newChan
  otid <- forkIO_ $ pipe_ yellConfig False chan o stdout
  etid <- forkIO_ $ pipe_ yellConfig True chan e stderr
  loud <- forkIO_ $ playSounds yellConfig chan

  waitForChildren_ [otid, etid] >> writeChan chan Nothing >> waitForChildren_ [loud]
  waitForProcess h

  return ()


-- | If argument is an existing file, return that.  Otherwise, search
-- $PATH.
executablePath :: FilePath -> IO (Maybe FilePath)
executablePath filepath = do
    exists :: Bool <- doesFileExist filepath
    if exists
        then return (Just filepath)
        else listToMaybe <$> (getSearchPath >>= filterM doesFileExist . map (</> filepath))


data YellConfig =
    YellConfig
      { yellConfigRules :: [(String, String)]
      , yellConfigPlayCmd :: String
      , yellConfigMaxNumSounds :: Int
      , yellConfigSoundsDir :: FilePath
      }
  deriving (Eq, Ord, Show)


-- | 'YellConfig' constructor.
loadYellConfig :: String -> IO YellConfig
loadYellConfig cmd = do
  dataDir  <- getDataDir
  homeDir  <- maybe "/" id . Prelude.lookup "HOME" <$> getEnvironment
  config   <- load $ map Optional [ dataDir </> "configs" </> cmd <.> "rc"
                                  , homeDir </> ".yell" </> cmd <.> "rc"
                                  , homeDir </> ".yell" </> "configs" </> cmd <.> "rc"
                                  ]

  let readRules :: Value -> [(String, String)]
      readRules (List xs) = map readRule xs
      readRules x = error $ "'rules' is not a list: " ++ show x

      readRule :: Value -> (String, String)
      readRule (List [String pattern, String soundfile]) = (cs pattern, cs soundfile)
      readRule x = error $ "'rules' has malformed entry: " ++ show x

  rules <- readRules <$> Conf.require config "rules"

  let readPlayCmd :: Value -> String
      readPlayCmd (String s) = cs s
      readPlayCmd x = error $ "'play-cmd' must be a string: " ++ show x

  playcmd <- readPlayCmd <$> Conf.require config "play-cmd"

  let readMuxNumSounds :: Value -> Int
      readMuxNumSounds x = case (convert x :: Maybe Int) of
                             (Just i) -> i
                             Nothing -> error $ "max-num-sounds must be an int: " ++ show x

  maxnumsounds <- readMuxNumSounds <$> Conf.require config "max-num-sounds"

  return $ YellConfig { yellConfigRules         = rules,
                        yellConfigPlayCmd       = playcmd,
                        yellConfigMaxNumSounds  = maxnumsounds,
                        yellConfigSoundsDir     = dataDir </> "sounds"
                      }


pipe_ :: YellConfig -> Bool -> Chan (Maybe String) -> Handle -> Handle -> IO ()
pipe_ YellConfig{..} loud chan hi ho = f
  where
    f = do
      eof <- hIsEOF hi
      if eof
          then return ()
          else do
              l <- hGetLine hi
              hPutStrLn ho l
              hFlush ho
              if loud
                  then do
                      writeChan chan (Just l) >> f
                  else do
                      f


forkIO_ :: IO () -> IO (MVar ())
forkIO_ computation = do
    mvar <- newEmptyMVar
    forkIO (computation `finally` (putMVar mvar ()))
    return mvar


waitForChildren_ :: Children -> IO ()
waitForChildren_ = mapM_ takeMVar


waitForChildrenMV_ :: ChildrenMV -> IO ()
waitForChildrenMV_ children = takeMVar children >>= f
  where
    f :: [MVar ()] -> IO ()
    f [] = return ()
    f (m:ms) = putMVar children ms >> takeMVar m >> waitForChildrenMV_ children


playSounds :: YellConfig -> Chan (Maybe String) -> IO ()
playSounds YellConfig{..} chan = getChanContents chan >>= mapM_ play . limit . catMaybes . map match . prune
  where
    prune :: [Maybe String] -> [String]
    prune = map fromJust . takeWhile isJust

    match :: String -> Maybe FilePath
    match line = listToMaybe . catMaybes $ map match_ yellConfigRules
      where
        match_ :: (String, String) -> Maybe String
        match_ (pattern, sound) = if line =~ pattern
                                    then Just $ yellConfigSoundsDir </> sound
                                    else Nothing

    limit :: [String] -> [String]
    limit = take yellConfigMaxNumSounds

    play :: FilePath -> IO ()
    play soundFile = do
      let subst :: String -> String -> String -> String
          subst pattern replacement source = subRegex (makeRegex pattern) source replacement

          (cmd:args) = map (subst "%s" soundFile) $ words yellConfigPlayCmd

      (i, o, e, h) <- runInteractiveProcess cmd args Nothing Nothing
      otid <- forkIO_ $ hGetContents o >> return ()
      etid <- forkIO_ $ hGetContents e >> return ()
      waitForChildren_ [otid, etid]
      waitForProcess h

      return ()

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Configurator as Conf
import Data.Configurator.Types
import Data.Maybe
import Data.String.Conversions
import Data.Typeable
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import Text.Regex
import Text.Regex.PCRE

import Paths_yell


type Children = [MVar ()]
type ChildrenMV = MVar Children


main :: IO ()
main = do
  yellConfig <- loadYellConfig
  args <- getArgs
  (i, o, e, h) <- runInteractiveProcess (yellConfigExec yellConfig) args Nothing Nothing

  chan <- newChan
  otid <- forkIO_ $ pipe_ yellConfig False chan o stdout
  etid <- forkIO_ $ pipe_ yellConfig True chan e stderr
  loud <- forkIO_ $ playSounds yellConfig chan

  waitForChildren_ [otid, etid] >> writeChan chan Nothing >> waitForChildren_ [loud]
  waitForProcess h

  return ()


-- | data type for the configurator data.  i felt i had to introduce
-- this, even if it is pretty bulky and boring, to put all the
-- messages about configuration errors in one place, and make the rest
-- of the code more concise and more robust.  i would like to know if
-- there is a better solution that i missed?
data YellConfig =
    YellConfig
      { yellConfigRules :: [(String, String)]
      , yellConfigPlayCmd :: String
      , yellConfigMaxNumSounds :: Int
      , yellConfigExec :: FilePath
      , yellConfigSoundsDir :: FilePath
      }
  deriving (Eq, Ord, Show)


-- | 'YellConfig' constructor.
loadYellConfig :: IO YellConfig
loadYellConfig = do
  progName <- getProgName
  dataDir  <- getDataDir
  home     <- maybe "/" id . Prelude.lookup "HOME" <$> getEnvironment
  config   <- load [Required $ dataDir </> progName <.> ".rc"]

  let g :: Value -> [(String, String)]
      g (List xs) = map f xs
      g x = error $ "'rules' is not a list: " ++ show x

      f :: Value -> (String, String)
      f (List [String pattern, String soundfile]) = (cs pattern, cs soundfile)
      f x = error $ "'rules' has malformed entry: " ++ show x

  rules <- g <$> Conf.require config "rules"

  let h :: Value -> String
      h (String s) = cs s
      h x = error $ "'play-cmd' must be a string: " ++ show x

  playcmd <- h <$> Conf.require config "play-cmd"

  let i :: Value -> Int
      i x = case (convert x :: Maybe Int) of
              (Just i) -> i
              Nothing -> error $ "max-num-sounds must be an int: " ++ show x

  maxnumsounds <- i <$> Conf.require config "max-num-sounds"

  let k :: Value -> String
      k (String s) = cs s
      k x = error "exec must be a file path string: " ++ show x

  exec <- k <$> Conf.require config "exec"

  return $ YellConfig { yellConfigRules         = rules,
                        yellConfigPlayCmd       = playcmd,
                        yellConfigMaxNumSounds  = maxnumsounds,
                        yellConfigExec          = exec,
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

{-# LANGUAGE RecordWildCards #-}
module Main where

-- hackage
import System.FilePath ( (</>), FilePath )
import Options.Applicative ( Parser, info, helper
                           , (<**>), fullDesc, progDesc, help
                           , execParser, long, short
                           , header, strOption, metavar, switch )
import System.Directory ( createDirectoryIfMissing, getHomeDirectory
                        , removeDirectoryRecursive )
import System.Process.Typed ( runProcess, runProcess_, proc )
import System.PosixCompat.User ( getEffectiveUserID )
-- base
import Control.Applicative (optional)
import System.Exit ( ExitCode(..), exitFailure )
import Data.List (last)
import Data.Maybe (maybe)
import Control.Monad (when)

data Option = Option
  { zipFile :: FilePath
  , dosboxConf :: Maybe FilePath
  , instName :: String
  , exePath :: FilePath
  , verbose :: Bool }

parser :: Parser Option
parser = Option
  <$> strOption ( long "zip-file" <> short 'z' <> metavar "PATH" <>
                  help "Path to zipfile that contains the dosbox program" )
  <*> (optional
        (strOption ( long "dosbox-conf" <> short 'c' <> metavar "PATH" <>
                     help "Path to dosbox conf for the dosbox program" )))
  <*> strOption ( long "instance-name" <> short 'n' <> metavar "NAME" <>
                  help "A unique name for a simple-dosbox-launcher instance" )
  <*> strOption ( long "exe-path" <> short 'p' <> metavar "PATH" <>
                  help "Relative path to the executable in zip-file" )
  <*> switch ( long "verbose" <> short 'v' <> help "verbose output" )

mountSafely :: FilePath -> [String] -> IO ()
mountSafely f args = do
  let target = last args
  procFail f args $ do
    ec <- runProcess $ proc "fusermount" ["-u", target]
    case ec of
      ExitFailure _ -> do
        putStrLn $ target ++ " cannot be unmounted."
        exitFailure
      ExitSuccess -> do
        procFail f args $ do
          putStrLn $ target ++ "cannot be mounted."
          exitFailure
  where procFail f args failf = do
          ec <- runProcess $ proc f args
          case ec of
            ExitSuccess -> return ()
            ExitFailure _ -> failf

main :: IO ()
main = do
  Option {..} <- execParser $ info (parser <**> helper)
    ( fullDesc <> progDesc "A simple dosbox launcher" <>
      header "simple-dosbox-launcher" )
  home <- getHomeDirectory
  uid <- getEffectiveUserID
  let tmpDir = "/tmp/simple-dosbox-launcher-" ++ show uid </> instName
      dataDir = home </> ".simple-dosbox-launcher"
      lower =  tmpDir </> "lower"
      merged = tmpDir </> "merged"
      upper = dataDir  </> instName
      confDir = home </> ".config" </> "simple-dosbox-launcher"
      userConf = confDir </> "dosbox-" ++ instName ++ ".conf"
  putStrLn $ "User dosbox configuration for "++instName++" : "++userConf
  putStrLn $ "User data for "++instName++" : "++upper
  when verbose $ do
    putStrLn $ zipFile++" will be mounted on "++lower
    putStrLn $ upper++" will be laid on top of "++lower++" at "++merged
    putStrLn $ merged </> exePath++" will be executed."
  putStrLn ""
  mapM_ (createDirectoryIfMissing True) [lower, merged, upper, confDir]
  mountSafely "fuse-zip" ["-r", zipFile, lower]
  mountSafely "unionfs" [ "-o", "cow,hide_meta_files"
                        , upper++"=RW:"++lower++"=RO", merged ]
  runProcess_ $ proc "dosbox" $
    concat [ ["-exit", "-userconf"]
           , maybe [] (\c -> ["-conf", c]) dosboxConf
           , [ "-conf", userConf
             , merged </> exePath ] ]
  runProcess_ $ proc "fusermount" ["-u", merged]
  runProcess_ $ proc "fusermount" ["-u", lower]
  removeDirectoryRecursive tmpDir

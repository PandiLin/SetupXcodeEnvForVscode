module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hReady)
import Control.Monad (forever)
import System.Environment (getExecutablePath)
import System.Exit (exitSuccess, ExitCode) 
import System.Process (readCreateProcess, shell, readCreateProcessWithExitCode)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.FilePath (takeExtension, (</>), takeDirectory)
import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List.Split (splitOn, chunk)


executeCommand :: String -> IO (ExitCode, String, String)
executeCommand cmd = readCreateProcessWithExitCode (shell cmd) ""


gitCommand :: String -> IO (ExitCode, String, String)
gitCommand cmd = executeCommand $ "git " ++ cmd

cloneXcodeBuildServer :: IO (ExitCode, String, String)
cloneXcodeBuildServer = gitCommand "clone https://github.com/SolaWing/xcode-build-server.git"


findFilesByExtension :: FilePath -> String -> IO [FilePath]
findFilesByExtension dir ext = do
     listDirectory dir >>=  filterM (\x -> return (takeExtension x == ext))


findXcodeProj :: FilePath -> IO [FilePath]
findXcodeProj dir = findFilesByExtension dir ".xcodeproj"


getExecutableDir :: IO FilePath
getExecutableDir = getExecutablePath <&> takeDirectory

confirmXcodeBuildServerExist :: FilePath -> IO Bool 
confirmXcodeBuildServerExist dir = doesFileExist (dir </> "xcode-build-server")

blankSpace :: String
blankSpace = " "



buildCommand :: [String] -> String
buildCommand = foldl splitCommand "" where
  splitCommand acc cmd = acc ++ blankSpace ++ cmd


configXcodeBuildServer ::  FilePath -> FilePath -> IO (ExitCode, String, String)
configXcodeBuildServer projectName dir = do 
  let xcodeBuildServerDir = dir </> "xcode-build-server"
  executeCommand $ buildCommand [xcodeBuildServerDir, "config", "-scheme", projectName, "-xcodeproj", projectName ++ ".xcodeproj"]


takeFileName :: FilePath -> String
takeFileName = head . splitOn "."

echo :: IO b
echo = do
  (keyEvent, fire) <- newAddHandler

  let network = fromAddHandler keyEvent >>= reactimate . fmap print

  compile network >>= actuate

  hSetBuffering stdin NoBuffering
  forever $ do 
    ready <- hReady stdin
    if ready
      then getChar >>= fire
      else return ()
 


main :: IO ()
main = do
  files <- getExecutableDir >>= findXcodeProj
  let filename = map (takeFileName)  files  
  print $ head filename


-- Helper function to create the greeting
greet :: String -> Int -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hReady)
import Control.Monad (forever)
import System.Environment (getExecutablePath)
import System.Exit (exitSuccess, ExitCode) 
import System.Process (readCreateProcess, shell, readCreateProcessWithExitCode, cwd)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.FilePath (takeExtension, (</>), takeDirectory)
import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List.Split (splitOn, chunk)


executeCommand :: String -> IO (ExitCode, String, String)
executeCommand cmd = readCreateProcessWithExitCode (shell cmd) ""


gitCommand :: String -> IO (ExitCode, String, String)
gitCommand cmd = executeCommand $ "git " ++ cmd

cloneXcodeBuildServer :: String -> String -> IO (ExitCode, String, String)
cloneXcodeBuildServer url destination = gitCommand $ buildCommand ["clone", url, destination]

cloneFromDefaultUrl :: String -> IO (ExitCode, String, String)
cloneFromDefaultUrl  = cloneXcodeBuildServer "https://github.com/SolaWing/xcode-build-server.git"

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
  currentDir <- getExecutableDir

  putStrLn "config xcode-build-server"
  existed <- confirmXcodeBuildServerExist currentDir
  putStrLn $ "xcode-build-server: " ++ show existed
  if existed
    then putStrLn "xcode-build-server already existed"
    else do
      putStrLn "clone xcode-build-server"
      _ <- cloneFromDefaultUrl currentDir
      putStrLn "done"

  putStrLn "config xcode-build-server"
  projectName <- (getExecutableDir >>= findXcodeProj) <&> (head . map takeFileName)
  putStrLn $ "current project name: " ++ projectName

  _ <- configXcodeBuildServer projectName currentDir
  putStrLn "done"
 




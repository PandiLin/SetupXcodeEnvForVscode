module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hReady)
import Control.Monad (forever)
import System.Environment (getExecutablePath)
import System.Exit (exitSuccess, ExitCode) 
import System.Process (readCreateProcess, shell, readCreateProcessWithExitCode, cwd, createProcess, proc, StdStream(..), std_out, std_err, waitForProcess)
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.FilePath (takeExtension, (</>), takeDirectory)
import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List.Split (splitOn, chunk)
import System.IO (hGetLine, hIsEOF)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import System.IO (Handle)
import System.Console.ANSI 

executeCommand :: String -> [String] -> IO ExitCode
executeCommand cmd args = do
    (_, Just hout, Just herr, ph) <- createProcess (proc cmd args){ std_out = CreatePipe, std_err = CreatePipe }
    
    void $ forkIO $ streamOutput hout
    void $ forkIO $ streamOutput herr
    
    waitForProcess ph

streamOutput :: Handle -> IO ()
streamOutput h = do
    eof <- hIsEOF h
    if eof
        then return ()
        else do
            line <- hGetLine h
            putStrLn line
            streamOutput h


gitCommand :: String -> [String] -> IO ExitCode
gitCommand cmd args = executeCommand "git" (cmd:args)

cloneXcodeBuildServer :: String -> String -> IO ExitCode
cloneXcodeBuildServer url destination = gitCommand "clone" [url, destination]

cloneFromDefaultUrl :: String -> IO ExitCode
cloneFromDefaultUrl destination = cloneXcodeBuildServer "https://github.com/SolaWing/xcode-build-server.git" destination

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


configXcodeBuildServer ::  FilePath -> FilePath -> IO ExitCode
configXcodeBuildServer projectName dir = do 
  let xcodeBuildServerDir = dir </> "xcode-build-server"
  executeCommand xcodeBuildServerDir ["config", "-scheme", projectName, "-project", projectName ++ ".xcodeproj"]


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
 

hintUser :: String -> IO ()
hintUser hint = do
  setSGR [SetColor Foreground Dull Green]
  putStrLn $ "==> " ++ hint
  setSGR [Reset]


main :: IO ()
main = do
  currentDir <- (getExecutableDir >>= return . (</> "xcode-build-server"))
  hintUser $ "current directory: " ++ currentDir

  hintUser "config xcode-build-server"
  existed <- confirmXcodeBuildServerExist currentDir
  hintUser $ "xcode-build-server existed: " ++ show existed
  if existed
    then hintUser "xcode-build-server already existed"
    else do
      putStrLn "clone xcode-build-server"
      _ <- cloneFromDefaultUrl currentDir
      putStrLn "done"

  hintUser "config xcode-build-server"
  projectName <- (getExecutableDir >>= findXcodeProj) <&> (head . map takeFileName)
  hintUser $ "current project name: " ++ projectName

  _ <- configXcodeBuildServer projectName currentDir
  hintUser "done"
 




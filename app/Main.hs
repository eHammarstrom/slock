module Main where

import Control.Exception as E
import System.IO
import System.Directory
import System.Exit
import System.Environment
import qualified Data.Text as T
import qualified Data.List as L

lineCount :: String -> Int
lineCount = length . filter (== '\n')

sortFilesAndDir :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
sortFilesAndDir check path = do
  ls <- listDirectory path
  bs <- mapM (\l -> check (path ++ l)) ls
  let lsbs = filter snd $ zip ls bs
  return $ map fst lsbs

getFiles :: FilePath -> IO [FilePath]
getFiles path = sortFilesAndDir doesFileExist path >>= \fs -> return $ map (path ++) fs

getDirs :: FilePath -> IO [FilePath]
getDirs = sortFilesAndDir doesDirectoryExist

getAllFiles' :: [FilePath] -> [FilePath] -> IO [FilePath]
getAllFiles' _ [] = return []
getAllFiles' ignoreDirs paths = do
  files <- mapM getFiles paths
  dirs  <- mapM getDirs paths

  let files' = concat files
  let dirs'  = concatMap prependPaths $ zip paths dirs
  let filteredDirs = filterDirs ignoreDirs dirs'

  files'' <- getAllFiles' ignoreDirs filteredDirs
  return $ files' ++ files''
  where
    prependPaths :: (FilePath, [FilePath]) -> [FilePath]
    prependPaths (p, ds) = map (\d -> p ++ d ++ "/") ds

    filterDirs :: [FilePath] -> [FilePath] -> [FilePath]
    filterDirs igds ds = filter (\d -> not $ or $ map (`isPrefixedWith` d) igds) ds

isPrefixedWith :: FilePath -> FilePath -> Bool
isPrefixedWith prefix cs = T.isPrefixOf (T.pack prefix) (T.pack cs)

isSuffixedWith :: FilePath -> FilePath -> Bool
isSuffixedWith suffix cs = T.isSuffixOf (T.pack suffix) (T.pack cs)

getAllFiles :: FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
getAllFiles path ignoreDirs ignoreFileEndings = do
  fs <- getAllFiles' ignoreDirs' [path ++ "/"]
  return $ filteredFiles ignoreFileEndings fs
  where
    ignoreDirs' = map (\d -> path ++ "/" ++ d ++ "/") ignoreDirs
    filteredFiles igfs fs = filter (\f -> not $ or $ map (`isSuffixedWith` f) igfs) fs

getFileSLOC :: FilePath -> IO Int
getFileSLOC f = readLineCount `E.catch` handler
  where
    readLineCount = do
      l <- lineCount <$> readFile f
      l `seq` return l

    handler :: SomeException -> IO Int
    handler _ = return 0

stripPrefix :: String -> String -> String
stripPrefix a b = case T.stripPrefix (T.pack a) (T.pack b) of
  Just x  -> T.unpack x
  Nothing -> b

parseOpt :: String -> Maybe String -> Maybe [String]
parseOpt opt arg = (words . stripPrefix opt) <$> arg

slock :: FilePath -> [FilePath] -> [FilePath] -> IO ()
slock p igds igfs = do
  fs    <- getAllFiles p igds igfs
  slocs <- mapM getFileSLOC fs
  putStrLn $ "sloc: " ++ show (sum slocs)

main :: IO ()
main = do
  args <- getArgs

  let igFileFlag  = "--ignore-files="
  let igDirFlag   = "--ignore-dirs="
  let ignoreFiles = parseOpt igFileFlag $ L.find (isPrefixedWith igFileFlag) args
  let ignoreDirs  = parseOpt igDirFlag $ L.find (isPrefixedWith igDirFlag) args

  case (length args, ignoreDirs, ignoreFiles) of
    (1, Nothing, Nothing)     -> slock (args !! 0) [] []
    (2, Just igds, Nothing)   -> slock (args !! 0) igds []
    (2, Nothing, Just igfs)   -> slock (args !! 0) [] igfs
    (3, Just igds, Just igfs) -> slock (args !! 0) igds igfs
    (_, _, _) -> do
      putStrLn "Usage: sloc PATH [--ignore-files=\"\"] [--ignore-dirs=\"\"]"
      exitFailure

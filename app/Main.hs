module Main where

import System.Directory
-- import Control.Monad
-- import Debug.Trace (trace)
import qualified Data.Text as T

type Dir'  = FilePath
type File' = FilePath

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
getAllFiles path ignoreDirs ignoreFileEndings =
  getAllFiles' ignoreDirs' [path ++ "/"] >>= \fs ->
  return $ filteredFiles ignoreFileEndings fs
  where
    ignoreDirs' = map (\d -> path ++ "/" ++ d ++ "/") ignoreDirs
    filteredFiles igfs fs = filter (\f -> not $ or $ map (`isSuffixedWith` f) igfs) fs

getFileSLOC :: FilePath -> IO Int
getFileSLOC f = lineCount <$> readFile f

main :: IO ()
main =
  readFile "test/langs/t.c" >>= print >>= \_ ->
  getAllFiles "./test/langs" [".stack-work", ".git"] [".o", ".out"] >>= \fs ->
  mapM getFileSLOC fs >>= print . sum

module Main where

import Control.Exception as E
import System.Directory
import System.Exit
import System.Environment
import Data.Maybe
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (dropWhile, dropWhileEnd)
import Data.Char (isSpace)

newtype SingleComment  = SC String
data MultiComment      = MC String String
data LangComment       = Lang SingleComment MultiComment

language :: FileEnding -> LangComment
language "hs" = Lang (SC "--") (MC "{-" "-}")
language _    = Lang (SC "//") (MC "/*" "*/")

type FileEnding       = String
type File             = String
type CommentNestDepth = Int

lineCount' :: LangComment -> [String] -> CommentNestDepth -> Int -> Int
lineCount' _ [] _ n                = n
lineCount' lc@(Lang (SC sc) (MC lmc rmc)) (l:ls) depth n
  | isPrefixedWith sc l    = lineCount' lc ls depth n
  | isPrefixedWith lmc l &&
    isSuffixedWith rmc  l  = lineCount' lc ls depth n
  | isPrefixedWith lmc l   = lineCount' lc ls (depth + 1) n
  | isPrefixedWith rmc l   = lineCount' lc ls (depth - 1) n
  | isSuffixedWith rmc l   = lineCount' lc ls (depth - 1) n
  | depth == 0             = lineCount' lc ls depth (n + 1)
  | otherwise              = lineCount' lc ls depth n

lineCount :: File -> FileEnding -> Int
lineCount f fe =
  let
    trim = dropWhileEnd isSpace . dropWhile isSpace
    lines' = (map trim . lines) f
    lang = language fe
  in lineCount' lang lines' 0 0

sortFilesAndDir :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
sortFilesAndDir check path = do
  ls <- listDirectory path
  bs <- mapM (\l -> check (path ++ l)) ls
  let lsbs = filter snd $ zip ls bs
  return $ map fst lsbs

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  fs <- sortFilesAndDir doesFileExist path
  return $ map (path ++) fs

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
    filterDirs igds = filter (\d -> not $ any (`isPrefixedWith` d) igds)

isPrefixedWith :: String -> String -> Bool
isPrefixedWith prefix cs = T.isPrefixOf (T.pack prefix) (T.pack cs)

isSuffixedWith :: String -> String -> Bool
isSuffixedWith suffix cs = T.isSuffixOf (T.pack suffix) (T.pack cs)

getAllFiles :: FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
getAllFiles path ignoreDirs ignoreFileEndings = do
  fs <- getAllFiles' ignoreDirs' [path ++ "/"]
  return $ filteredFiles ignoreFileEndings fs
  where
    ignoreDirs' = map (\d -> path ++ "/" ++ d ++ "/") ignoreDirs
    filteredFiles igfs = filter (\f -> not $ any (`isSuffixedWith` f) igfs)

getFileSLOC :: FilePath -> IO Int
getFileSLOC f = readLineCount `E.catch` handler
  where
    fileEnding = T.unpack . last . T.split (== '.') . T.pack

    readLineCount = do
      l <- (`lineCount` fileEnding f) <$> readFile f
      l `seq` return l

    handler :: SomeException -> IO Int
    handler _ = return 0

stripPrefix :: String -> String -> String
stripPrefix a b = T.unpack $ fromMaybe (T.pack "") $ T.stripPrefix (T.pack a) (T.pack b)

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

  let igFTypesFlag = "--ignore-ftypes="
  let igDirFlag    = "--ignore-dirs="
  let ignoreFiles  = fromMaybe [] $ parseOpt igFTypesFlag $ L.find (isPrefixedWith igFTypesFlag) args
  let ignoreDirs   = fromMaybe [] $ parseOpt igDirFlag  $ L.find (isPrefixedWith igDirFlag) args
  let path         = filter (not . isPrefixedWith "--") args

  case path of
    [path'] -> slock path' ignoreDirs ignoreFiles
    _       -> do
      putStrLn "Usage: slock PATH [OPTIONS]\n\
               \Available options:\n\
               \\t --ignore-ftypes\t\t List file types to ignore, e.g. --ignore-ftypes=\".o .out .cpp\"\n\
               \\t --ignore-dirs\t\t\t List directories to ignore, e.g. --ignore-dirs=\".node-modules test\""
      exitFailure

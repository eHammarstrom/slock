module Main where

import           Control.Exception            as E
import           Data.Char                    (isSpace)
import           Data.List                    (dropWhile, dropWhileEnd)
import qualified Data.List                    as L
import           Data.Maybe
import qualified Data.Text                    as T
import           System.Directory
import           System.Environment
import           System.Exit

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Par.Combinator (parMapM)
import           Control.Monad.Par.IO         (runParIO)

import           Control.Monad.State.Lazy
import           Text.Regex.Posix

type RegexStr         = String
type FileEnding       = String
type File             = String
type CommentNestDepth = Int

data SState = SState { root_path    :: String
                     , files        :: ![File]
                     , ignore_regxs :: [RegexStr]
                     }

newtype SingleComment  = SC String
data MultiComment      = MC String String
data LangComment       = Lang SingleComment MultiComment

language :: FileEnding -> LangComment
language "hs" = Lang (SC "--") (MC "{-" "-}")
language _    = Lang (SC "//") (MC "/*" "*/")

nestCount :: MultiComment -> String -> String -> CommentNestDepth
-- the parse walk, build buffer, clear when finding term
nestCount mc@(MC lmc rmc) (c:cs) buffer
  | isSuffixedWith lmc buffer  = nestCount mc cs [] + 1
  | isSuffixedWith rmc buffer  = nestCount mc cs [] - 1
  | otherwise                  = nestCount mc cs (buffer ++ [c])
nestCount mc@(MC lmc rmc) [] buffer
  | isSuffixedWith lmc buffer  = nestCount mc [] [] + 1
  | isSuffixedWith rmc buffer  = nestCount mc [] [] - 1
  | otherwise                  = 0

lineCount' :: LangComment -> [String] -> CommentNestDepth -> Int
lineCount' _ [] _                = 0
lineCount' lc@(Lang (SC sc) mc@(MC lmc rmc)) (l:ls) depth
  -- // hello world
  | isPrefixedWith sc l          = lineCount' lc ls depth
  -- /* hello world */
  | isPrefixedWith lmc l &&
    isSuffixedWith rmc l         = lineCount' lc ls depth
  -- trailing multiline fix, otherwise: someCode */ == someCode /* end comment */
  | depth + nc == 0 && nonEmpty &&
    isSuffixedWith rmc l &&
    not (isInfixedWith lmc l)    = lineCount' lc ls 0
  -- printf("%d\n,/* hello world */ 10);
  -- printf("%d\n", 10);/* hello world */
  -- /* Hello world */printf("%d\n, 10);
  | depth + nc == 0 && nonEmpty  = lineCount' lc ls 0 + 1
  | otherwise                    = lineCount' lc ls (depth + nc)
  where
    nc        = nestCount mc l ""
    nonEmpty  = l /= []

lineCount :: File -> FileEnding -> Int
lineCount f fe =
  let
    trim = dropWhileEnd isSpace . dropWhile isSpace
    lines' = (map trim . lines) f
    lang = language fe
  in lineCount' lang lines' 0

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
  -- files <- mapM getFiles paths
  files <- runParIO $ parMapM (\p -> liftIO $ getFiles p) paths
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

isInfixedWith :: String -> String -> Bool
isInfixedWith as bs = T.isInfixOf (T.pack as) (T.pack bs)

isPrefixedWith :: String -> String -> Bool
isPrefixedWith as bs = T.isPrefixOf (T.pack as) (T.pack bs)

isSuffixedWith :: String -> String -> Bool
isSuffixedWith as bs = T.isSuffixOf (T.pack as) (T.pack bs)

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
  -- count sloc over all files
  -- slocs <- getFileSLOC
  slocs <- runParIO $ parMapM (\f -> liftIO $ getFileSLOC f) fs

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

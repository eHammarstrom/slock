{-# LANGUAGE OverloadedStrings #-}

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

data StateRec = StateRec { root_path    :: String
                         , files        :: ![File]
                         , ignore_regxs :: [RegexStr]
                         }

type SState a = StateT StateRec IO a

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

regCheck :: FilePath -> [RegexStr] -> Bool
regCheck x = any (==True) . map (\p -> x =~ p :: Bool)

getAllFiles :: [FilePath] -> SState ()
getAllFiles [] = return ()
getAllFiles paths = do
  s <- get
  files <- liftIO . runParIO $ parMapM (\p -> liftIO $ getFiles p) paths
  dirs  <- liftIO . runParIO $ parMapM (\d -> liftIO $ getDirs d)  paths

  let files' = concat files
  let dirs'  = concatMap prependPaths $ zip paths dirs

  let ignored' = flip regCheck $ ignore_regxs s

  let filteredFiles = filter (not . ignored') files'
  let filteredDirs = filter (not . ignored') dirs'

  addFiles filteredFiles

  getAllFiles filteredDirs
  where
    prependPaths :: (FilePath, [FilePath]) -> [FilePath]
    prependPaths (p, ds) = map (\d -> p ++ d ++ "/") ds

isInfixedWith :: String -> String -> Bool
isInfixedWith as bs = T.isInfixOf (T.pack as) (T.pack bs)

isPrefixedWith :: String -> String -> Bool
isPrefixedWith as bs = T.isPrefixOf (T.pack as) (T.pack bs)

isSuffixedWith :: String -> String -> Bool
isSuffixedWith as bs = T.isSuffixOf (T.pack as) (T.pack bs)

searchFiles :: SState ()
searchFiles = do
  s <- get
  getAllFiles [root_path s]

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

addFiles :: [FilePath] -> SState ()
addFiles fs = do
  s <- get
  put $ s { files = ( files s ) ++ fs }

slock :: SState ()
slock = do
  searchFiles

  s <- get

  -- count sloc over all files
  slocs <- liftIO . runParIO $ parMapM (\f -> liftIO $ getFileSLOC f) (files s)

  liftIO . putStrLn $ "sloc: " ++ show (sum slocs)

main :: IO ()
main = do
  args <- getArgs

  let path = filter (not . isPrefixedWith "--") args

  case path of
    [path'] -> do
      let initial_state = StateRec { root_path = path'
                                   , files = []
                                   , ignore_regxs = []
                                   }

      evalStateT slock initial_state

    _       -> do
      putStrLn "Usage: slock PATH\n"
      exitFailure

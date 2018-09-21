{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception            as E
import           Data.Char                    (isSpace)
import           Data.List                    (dropWhile, dropWhileEnd)
import qualified Data.List                    as L
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Prelude                      hiding (FilePath)
import           System.Directory
import           System.Environment
import           System.Exit

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Par.Combinator (parMapM)
import           Control.Monad.Par.IO         (runParIO)

import           Control.Monad.State.Lazy
import           Text.Regex.Posix

type RegexStr         = String
type FileEnding       = Text
type File             = Text
type FilePath         = Text
type CommentNestDepth = Int

data StateRec = StateRec { root_path    :: Text
                         , files        :: ![File]
                         , ignore_regxs :: [RegexStr]
                         }

type SState a = StateT StateRec IO a

newtype SingleComment  = SC Text

data MultiComment      = MC Text Text
data LangComment       = Lang SingleComment MultiComment

language :: FileEnding -> LangComment
language "hs" = Lang (SC "--") (MC "{-" "-}")
language _    = Lang (SC "//") (MC "/*" "*/")

nestCount :: MultiComment -> Text -> Text -> CommentNestDepth
-- the parse walk, build buffer, clear when finding term
nestCount mc@(MC lmc rmc) txt buffer
  | T.null txt              = 0
  | T.isSuffixOf lmc buffer = nestCount mc cs T.empty + 1
  | T.isSuffixOf rmc buffer = nestCount mc cs T.empty - 1
  | otherwise               = nestCount mc cs (T.snoc buffer c)
    where cs = T.tail txt
          c  = T.head txt

lineCount' :: LangComment -> [Text] -> CommentNestDepth -> Int
lineCount' _ [] _                = 0
lineCount' lc@(Lang (SC sc) mc@(MC lmc rmc)) (l:ls) depth
  -- // hello world
  | T.isPrefixOf sc l          = lineCount' lc ls depth
  -- /* hello world */
  | T.isPrefixOf lmc l &&
    T.isSuffixOf rmc l         = lineCount' lc ls depth
  -- trailing multiline fix, otherwise: someCode */ == someCode /* end comment */
  | depth + nc == 0 && nonEmpty &&
    T.isSuffixOf rmc l &&
    not (T.isInfixOf lmc l)    = lineCount' lc ls 0
  -- printf("%d\n,/* hello world */ 10);
  -- printf("%d\n", 10);/* hello world */
  -- /* Hello world */printf("%d\n, 10);
  | depth + nc == 0 && nonEmpty  = lineCount' lc ls 0 + 1
  | otherwise                    = lineCount' lc ls (depth + nc)
  where
    nc        = nestCount mc l ""
    nonEmpty  = not . T.null $ l

lineCount :: File -> FileEnding -> Int
lineCount f fe =
  let
    trim = T.dropWhileEnd isSpace . T.dropWhile isSpace
    lines' = (map trim . T.lines) f
    lang = language fe
  in lineCount' lang lines' 0

sortFilesAndDir :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
sortFilesAndDir check path = do
  let spath = T.unpack path
  ls <- map T.pack <$> listDirectory spath
  bs <- mapM (\l -> check (T.append path l)) ls
  let lsbs = filter snd $ zip ls bs
  return $ map fst lsbs

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  fs <- sortFilesAndDir (doesFileExist . T.unpack) path
  return $ map (T.append path) fs

getDirs :: FilePath -> IO [FilePath]
getDirs = sortFilesAndDir (doesDirectoryExist . T.unpack)

regCheck :: Text -> [RegexStr] -> Bool
regCheck x = any (==True) . map (\p -> x' =~ p :: Bool)
  where x' = T.unpack x :: String

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
    prependPaths (p, ds) = map (\d -> T.append (T.append p d) "/") ds

searchFiles :: SState ()
searchFiles = do
  s <- get
  getAllFiles [root_path s]

getFileSLOC :: FilePath -> IO Int
getFileSLOC f = readLineCount `E.catch` handler
  where
    fileEnding = last . T.split (== '.')

    readLineCount = do
      l <- (`lineCount` fileEnding f) <$> T.pack <$> readFile (T.unpack f)
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
  args <- map T.pack <$> getArgs

  let path = filter (not . T.isPrefixOf "--") args

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

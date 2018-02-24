module Main where

import System.Directory
import Control.Monad

-- import Lib

type Dir'  = FilePath
type File' = FilePath

lineCount :: String -> Int
lineCount = length . filter (== '\n')

sortFilesAndDir :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
sortFilesAndDir check path = do
  ls <- listDirectory path
  bs <- mapM check ls
  let lsbs = filter snd $ zip ls bs
  return $ map fst lsbs

getFiles :: FilePath -> IO [FilePath]
getFiles = sortFilesAndDir doesFileExist

getDirs :: FilePath -> IO [FilePath]
getDirs = sortFilesAndDir doesDirectoryExist

main :: IO ()
main =
  readFile "test/langs/t.c" >>= \f ->
  print f                   >>= \_ ->
  print (lineCount f)       >>= \_ ->
  getFiles "." >>= print    >>= \_ ->
  getDirs "."               >>= print


{- notes
listDirectory "."         >>= \ls ->
sequence_ $ ((>>= print) . doesFileExist) <$> ls
mapM_ (doesFileExist >=> print) ls
-}

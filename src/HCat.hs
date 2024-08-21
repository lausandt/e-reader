{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HCat where

import qualified Control.Exception as Exception
-- https://wiki.haskell.org/Dealing_with_binary_data
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time as TimeFormat
import qualified Data.Time.Clock as Clock
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO (BufferMode (NoBuffering), IOMode (ReadMode), hGetChar, hSetBuffering, hSetEcho, openFile, stdin, stdout)
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf (printf)
import System.Console.ANSI (clearScreen)


data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data Control = Forward | Back | Stop | Help
  deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWriteable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

runHCat :: IO ()
runHCat = do
  targetFilePath <- do
    args <- handleArgs
    eitherToErr args

  contents <- do
    fileHandle <- openFile targetFilePath ReadMode
    TextIO.hGetContents fileHandle

  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath

  let pages = paginate termSize finfo contents
  showPages 0 pages

-- (<$>) / fmap :: Functor f => (a -> b) -> f a -> f b
handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArguments <$> Env.getArgs
  where
    parseArguments argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "no arguments provided"
        _ -> Left "multiple files not supported"

eitherToErr :: (Show a) => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardWrappedText textIndex
      | textIndex <= 0 = (hardWrappedText, Text.empty)
      | Text.index hardWrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardWrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardWrappedText (textIndex - 1)

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let rows' = rows - 1
      wrappedLines = concatMap (wordWrap cols) (Text.lines text)
      pages = map (Text.unlines . padTo rows') (groupsOf rows' wrappedLines)
      pageCount = length pages
      statusLine = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLine
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad = take lineCount $ rowsToPad <> repeat ""

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      lines <- readProcess "tput" ["lines"] ""
      cols <- readProcess "tput" ["cols"] ""

      let lines' = read $ init lines
          cols' = read $ init cols
      return $ ScreenDimensions lines' cols'

getContinue :: IO Control
getContinue =
  hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \case
      ' ' -> return Forward
      'b' -> return Back 
      'q' -> return Stop
      '?' -> return Help
      _ -> getContinue 


showPages :: Int -> [Text.Text] -> IO ()
showPages _ [] = return ()
showPages n pages =
  if n >= 0 then 
    clearScreen
      >> TextIO.putStrLn (pages !! n)
      >> getContinue
      >>= \case
        Back -> showPages (n - 1) pages 
        Forward -> showPages (n + 1) pages
        Stop -> return ()
        Help -> showHelp
  else
    clearScreen
      >> TextIO.putStrLn (head pages) 
      >> getContinue
      >>= \case
        Back -> showPages 0 pages 
        Forward -> showPages (n + 1) pages
        Stop -> return ()
        Help -> showHelp 

-- showHelp :: Int -> [Text.Text] -> IO ()
-- showHelp :: IO ()
showHelp = do
  handle <-  openFile "help.txt" ReadMode
  contents <- TextIO.hGetContents handle
  _ <- TextIO.putStrLn contents
  return ()
  



-- clearScreen :: IO ()
-- -- Using ByteString is conceptually sound as an escape sequence is a ByteString
-- clearScreen = BS.putStr "\^[[IJ\^[[1;1h"

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return
    FileInfo
      { filePath = filePath,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWriteable = Directory.writable perms,
        fileExecutable = Directory.executable perms
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      permissionString =
        [ if fileReadable then 'r' else '-',
          if fileWriteable then 'w' else '-',
          if fileExecutable then 'x' else '-'
        ]
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
   in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

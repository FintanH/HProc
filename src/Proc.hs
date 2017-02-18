{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Proc where

import           Control.Monad    (filterM)
import           Data.Char        (isDigit)
import           Data.List.Split  (splitOn)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import qualified Data.Text        as T
import           System.Directory (doesDirectoryExist, listDirectory)
import           Text.Printf


newtype PID = PID String
type ProcInfo = M.Map T.Text [T.Text]
type UserInfo = M.Map T.Text T.Text

data ProcessData = ProcessData {
  pid   :: FilePath,
  name  :: Maybe T.Text,
  user  :: Maybe T.Text,
  state :: Maybe T.Text,
  rss   :: Maybe T.Text
}

instance Show ProcessData where
  show ProcessData{..} =
    (printf format ("PID" :: String) ("NAME" :: String) ("USER" :: String) ("STATE" :: String) ("MEMORY" :: String)) <>
    (printf format ("------" :: String) ("---------------" :: String) ("----------" :: String) ("----------" :: String) ("----------" :: String)) <>
    (printf format) pid (fromMaybe "" name) (toString user) (toString state) (toString rss)
    where
      format = "%6s\t%-15s\t%-10s\t%-10s\t%-10s\n"
      toString :: Maybe T.Text -> String
      toString = fromMaybe "" . fmap T.unpack

procFolder :: FilePath
procFolder = "/proc"

getCurrentPIDs :: IO [FilePath]
getCurrentPIDs = do
  dirs <- listDirectory procFolder
  filterM isProcessFolder dirs

isProcessFolder :: FilePath -> IO Bool
isProcessFolder folder = do
  isDir <- doesDirectoryExist $ procFolder <> "/" <> folder
  let isNum = (all isDigit folder) && folder /= "0"
  return $ isDir && isNum

getProcessData :: FilePath -> IO ProcInfo
getProcessData pid = do
  let pidPath = procFolder <> "/" <> pid <> "/status"
  M.fromList . groupData . T.pack <$> readFile pidPath
  where
    groupData = map (\d -> (head d, map T.strip $ tail d)) . map (T.splitOn "\t") . T.lines

getUsers :: IO UserInfo
getUsers = do
  let userPath = "/etc/passwd"
  userList <- removeCommentLines . groupData . T.strip . T.pack <$> readFile userPath
  return . M.fromList . map (\u -> (u !! 2, u !! 0)) $ userList
  where
    groupData = map (T.splitOn ":") . T.lines
    removeCommentLines = filter (not . null) . map (filter (not . T.isPrefixOf "#"))

createProcess :: FilePath -> IO ProcessData
createProcess pid = do
  pData <- getProcessData pid
  let name = head <$> M.lookup "Name:" pData :: Maybe T.Text
  let userId = head <$> M.lookup "Uid:" pData :: Maybe T.Text
  let state = head <$> M.lookup "State:" pData :: Maybe T.Text
  let rss = head <$> M.lookup "VMRSS:" pData :: Maybe T.Text

  userInfo <- getUsers
  let user = userId >>= \ui -> M.lookup ui userInfo

  return ProcessData {..}

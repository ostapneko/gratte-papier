{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import Gratte.Document
import Gratte.Options
import qualified OldDocument as OD
import Data.Aeson
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = do
    files <- getDirectoryRecurse [FS.decodeString "/home/vagrant/gratte-cleaning/files"] []
    let metadataFiles = filter (flip FS.hasExtension $ T.pack "metadata") files
    mapM_ correctFile metadataFiles

getDirectoryRecurse :: [FS.FilePath] -> [FS.FilePath] -> IO [FS.FilePath]
getDirectoryRecurse [] acc = return acc
getDirectoryRecurse (dir:dirs) acc = do
    children <- FS.listDirectory dir
    newFiles <- filterM FS.isFile children
    newDirs  <- filterM FS.isDirectory children
    getDirectoryRecurse (dirs ++ newDirs) (acc ++ newFiles)

correctFile :: FS.FilePath -> IO ()
correctFile path = do
    let metadataPath = FS.replaceExtension path "json"
    doesNewFileExist <- FS.isFile metadataPath
    unless doesNewFileExist $ do
      putStrLn $ "Correcting file: " ++ FS.encodeString path
      oldContent <- T.unpack `liftM` FS.readTextFile path
      let oldDoc = read oldContent :: OD.OldDocument

      let newHash        = let (OD.DocumentHash hash) = OD.hash oldDoc in DocumentHash hash
          newPath    = DocumentPath $ OD.filepath oldDoc
          newTags        = OD.tags oldDoc
          newScannedText = OD.freeText oldDoc

      putStrLn $ "Metadata content:"
      putStrLn $ show oldDoc

      putStrLn "New Title: "
      newTitle <- DocumentTitle `liftM` getLine

      putStrLn "Sender: "
      newSender <- DocumentSender `liftM` getLine

      putStrLn "Recipient: "
      newRecipient <- DocumentRecipient `liftM` getLine

      putStrLn "Date: "
      newDate <- do
        input <- getLine
        return $ either (\ (InvalidOptions err) -> error $ "error: " ++ err) id (parseDate input)

      let newDoc = Document {
              docHash        = newHash
            , docTitle       = newTitle
            , docPath        = newPath
            , docSender      = newSender
            , docRecipient   = newRecipient
            , docDate        = newDate
            , docTags        = newTags
            , docScannedText = newScannedText
            }

      putStrLn "New metadata:"
      LBS.putStrLn $ encode newDoc
      confirmation <- askForConfirmation True
      if confirmation
        then FS.writeFile metadataPath $ BS.concat . LBS.toChunks $ encode newDoc
        else putStrLn "Aborted!"

      putStrLn "-----------------------------------------------"

askForConfirmation :: Bool    -- ^ Is it the first time we ask the question ?
                   -> IO Bool
askForConfirmation isFirstTime = do
  let msg = if (isFirstTime)
              then "Do you want to procede? [y/N]"
              else "Please type \"y\" or \"n\""
  putStrLn msg
  answer <- getLine
  case map toLower answer of
    "y" -> return True
    "n" -> return False
    _   -> askForConfirmation False

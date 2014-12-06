{-# LANGUAGE OverloadedStrings #-}

module Gratte.Command.Encryption where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte
import Data.Maybe
import Data.Monoid
import Gratte.Options
import Gratte.Options.Encryption
import Gratte.Utils
import Filesystem.Path.CurrentOS ( (</>), (<.>) )
import Crypto.Random.DRBG
import Crypto.Cipher.AES
import qualified Data.ByteString.Char8     as BS
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

-- ^ Get a password from the input and transform it
-- into a 16 bytes bytestring (if necessary
-- by repeating and truncating the input)
getPassword :: IO BS.ByteString
getPassword = do
  putStrLn "Please enter your password"
  strPass <- getLine
  let pass = BS.pack strPass
      go s = if BS.length s >= 32 then BS.take 32 s else go (s <> s)
  return $ go pass

sync :: FS.FilePath -- ^ Source folder
     -> FS.FilePath -- ^ Target folder
     -> (FS.FilePath -> FS.FilePath) -- ^ Basename transformation from the
                                     -- source to the target
     -> (FS.FilePath -> FS.FilePath -> IO ()) -- ^ Transformation from source to target
     -> IO (Int, Int) -- ^ (Number of synchronised files, total number of files)
sync sourceDir targetDir nameTrans fileTrans = do
  -- /path/to/dir -> /path/to/dir/
  -- otherwise stripPrefix doesn't work as it should
  let sourceDir' = FS.decodeString $ FS.encodeString sourceDir ++ "/"
  sourceFiles <- getFilesRecurs sourceDir'
  let sourcePaths = map (FS.stripPrefix sourceDir') sourceFiles

  -- keep the files that either don't have a twin, or a twin that
  -- is less recent
  toSync <- flip filterM (map fromJust sourcePaths) $ \p -> do
    let sourceFile = sourceDir' </> p
        targetFile = nameTrans $ targetDir </> p
    existTarget <- FS.isFile targetFile
    if existTarget
      then
        liftM2 (>) (FS.getModified sourceFile) (FS.getModified targetFile)
      else return True

  let filePairs = map (\p ->
                        ( sourceDir' </> p
                        , nameTrans (targetDir </> p)
                        )
                      )
                      toSync

  mapM_ (uncurry fileTrans) filePairs
  return (length toSync, length sourcePaths)

encryptFiles :: EncryptionOptions -> Gratte ()
encryptFiles encOpts = do
  sourceDir <- getOption folder
  let targetDir = encryptFolder encOpts
  password <- liftIO getPassword
  let nameTrans = (<.> "enc")
  (synced, total) <- liftIO $ sync sourceDir targetDir nameTrans (encryptFile password)
  logNotice $ show synced ++ "/" ++ show total ++ " files encrypted"

decryptFiles :: EncryptionOptions -> Gratte ()
decryptFiles encOpts = do
  let sourceDir = encryptFolder encOpts
  targetDir <- getOption folder
  password <- liftIO getPassword
  opts <- getOptions
  let logFailure s = withGratte opts $ logError $ "Error while decrypting " ++ FS.encodeString s
  let write s t = do
                    isSuccess <- decryptFile password s t
                    unless isSuccess $ logFailure s
                    return ()
  (synced, total) <- liftIO $ sync sourceDir targetDir FS.dropExtension write
  logNotice $ show synced ++ "/" ++ show total ++ " files decrypted"

encryptFile :: BS.ByteString -- ^ The password
            -> FS.FilePath   -- ^ File to encrypt
            -> FS.FilePath   -- ^ Target
            -> IO ()
encryptFile pass source target = do
  decContent <- FS.readFile source
  let encContent = encrypt pass decContent
  FS.createTree $ FS.directory target
  FS.writeFile target =<< encContent

decryptFile :: BS.ByteString -- ^ The password
            -> FS.FilePath   -- ^ File to decrypt
            -> FS.FilePath   -- ^ Target
            -> IO Bool       -- ^ Success?
decryptFile pass source target = do
  encContent <- FS.readFile source
  let mDecContent = decrypt pass encContent
  case mDecContent of
    Nothing -> return False
    Just decContent -> do
      FS.createTree $ FS.directory target
      FS.writeFile target decContent
      return True

encrypt :: BS.ByteString    -- ^ The password (32 bytes)
        -> BS.ByteString    -- ^ The clear bytestring
        -> IO BS.ByteString -- ^ The encrypted bytestring
encrypt pass decContent = do
  let cipher = initAES pass
  ivStr <- randomByteString 16 -- The initialization vector for the CBC encryption
  let iv        = aesIV_ ivStr
      encrypted = encryptCBC cipher iv $ pad decContent
  return $ ivStr <> wrapStart <> encrypted

decrypt :: BS.ByteString       -- ^ The password (32 bytes)
        -> BS.ByteString       -- ^ The encrypted bytestring, with the form
                               -- thisistheivBEGIN_CONTENTencryptedcontent
        -> Maybe BS.ByteString -- ^ The decrypted bytestring
decrypt pass wrapped =
  let (ivStr, prefixed) = BS.breakSubstring wrapStart wrapped
      enc               = BS.drop (BS.length wrapStart) prefixed
      cipher            = initAES pass
      iv                = aesIV_ ivStr
      padded            = decryptCBC cipher iv enc
  in  unpad padded

wrapStart :: BS.ByteString
wrapStart = "BEGIN_CONTENT"

wrapEnd :: BS.ByteString
wrapEnd = "END_CONTENT"

-- | wrap the content like so: bla -> blaEND_CONTENT00000
-- So that the whole bytestring has a size multiple of 16
pad :: BS.ByteString -> BS.ByteString
pad str =
  let suffixed = str <> wrapEnd
      l        = BS.length suffixed
  in suffixed <> BS.replicate (16 - l `rem` 16) '0'

unpad :: BS.ByteString -> Maybe BS.ByteString
unpad str =
  let unpadded = BS.reverse . BS.dropWhile (== '0') . BS.reverse $ str
  in  if wrapEnd `BS.isSuffixOf` unpadded
        then Just $ BS.take (BS.length unpadded - BS.length wrapEnd) unpadded
        else Nothing

-- | Random hexa ByteString of size n generator
randomByteString :: Int -> IO BS.ByteString
randomByteString n = do
  gen <- newGenIO :: IO CtrDRBG
  case genBytes n gen of
    Right (bs, _) -> return $ BS.take n bs
    Left _        -> error "Error while trying to generate a random string!"

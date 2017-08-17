#!/usr/bin/env runhaskell

-- My personal filter for writing labnotes.

import Text.Pandoc.JSON
import Text.Pandoc
import System.Process
import Data.Monoid
import Data.List (find)
import Control.Concurrent
import Data.List
import Data.List.Split 
import System.IO
import System.Directory

-- Create a local copy of included inmages in pandoc. As long as local copy
-- exists, do not use the file from given link.
getFilename :: String -> String 
getFilename path = last $ splitOn "/" path 

tempDirName = "_copy_of_images"

copyFileIfDoesNotExists oldFile newFile = do
    isFile <- doesFileExist newFile
    if not isFile
        then copyFile oldFile newFile
        else return ()

copyImagesLocally :: Inline -> IO Inline
copyImagesLocally (Image attr xs tgt) = do 
    let filename = getFilename $ fst tgt
    createDirectoryIfMissing True tempDirName
    let newPath = tempDirName ++ "/" ++ filename
    -- Copy this file to local destination only if this file does not exists.
    copyFileIfDoesNotExists (fst tgt) newPath
    {-hPutStrLn stderr $ filename-}
    return $ Image attr xs (newPath, snd tgt)

copyImagesLocally x = return x

labnote x = do 
    x1 <- copyImagesLocally x
    return x1

main :: IO ()
main = do 
    toJSONFilter labnote

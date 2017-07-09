#!/usr/bin/env runhaskell

-- Create a local copy of included inmages in pandoc. As long as local copy
-- exists, do not use the file from given link.

import Text.Pandoc.JSON
import Data.List.Split 
import System.IO
import System.Directory

main :: IO ()
main = toJSONFilter copyImagesLocally

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

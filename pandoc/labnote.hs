#!/usr/bin/env runhaskell

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


-- | Filter to handle commands.
handleCmds :: Block -> IO Block
handleCmds (CodeBlock (ident, [] , (("showCmdBlock",cmd0):attrs)) code) = do
    cmdBlock <- handleCmds $ CodeBlock (ident, [], (("cmdBlock",cmd0):attrs)) code
    return $ Div ("", ["groupCodeBlock"], [])
                 [ CodeBlock (ident, ["bash"], []) $ "$ " <> cmd0
                 , cmdBlock
                 ]
handleCmds (CodeBlock (ident, [] , (("cmdBlock",cmd0):attrs)) code) = do
    let (cmd:args) = words cmd0
    let outClasses =
              words
            . maybe [] snd
            . find ((== "outClasses") . fst)
            $ attrs
    let inClasses =
              words
            . maybe [] snd
            . find ((== "inClasses") . fst)
            $ attrs
    let inputBlock =
            if null code
            then Null
            else CodeBlock (ident, inClasses, []) code
    result <- readProcess cmd args code
    return $ Div ("", ["groupCodeBlock"], [])
                 [ inputBlock
                 , CodeBlock (ident, outClasses, []) result
                 ]
handleCmds x = return x

-- | Filter to insert diagram.
insertDiagrams :: Block -> IO Block
insertDiagrams (CodeBlock (ident, ["diagram"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    writeFile "tmp.hs" code
    system "ghc -O2 tmp.hs -o tmp -rtsopts > /dev/null"
    system "./tmp +RTS -K100M > /dev/null"
    return $ Para $ [Image nullAttr [Str "test"] (outFile,"test title")]
insertDiagrams (CodeBlock (ident, ["diagram","haskell"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    writeFile "tmp.hs" code
    system "ghc -O2 tmp.hs -o tmp -rtsopts > /dev/null"
    system "./tmp +RTS -K100M > /dev/null"
    return $ Div ("",[],[]) [(CodeBlock (ident, ["haskell"] , attrs) code)
                            ,Para $ [Image nullAttr [Str "test"] (outFile,"test title")]
                            ]
insertDiagrams x = return x

-- | filter to insert dot file.
-- | Poor mans function to get a file extension
getFileExtension fileName = reverse ext
  where
    (ext, _) = break (=='.') . reverse $ fileName

insertDot :: Block -> IO Block
insertDot (CodeBlock (ident, ["showDot"] , attrs) code) = do
    writeFile "log" $ show attrs
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    let ext = getFileExtension outFile

    writeFile "tmp.dot" code
    system $ "dot tmp.dot -T" ++ ext ++ " > " ++ outFile
    return $ Para $ [Image nullAttr [Str "test"] (outFile,"test title")]

insertDot (CodeBlock (ident, ["showDotCode"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    let ext = getFileExtension outFile

    writeFile "tmp.dot" code
    system $ "dot tmp.dot -T" ++ ext ++ " > " ++ outFile
    return $ Div ("",[],[]) [(CodeBlock (ident, ["haskell"] , attrs) code)
                            ,Para $ [Image nullAttr [Str "test"] (outFile,"test title")]
                            ]
insertDot x = return x


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


main :: IO ()
main = do 
    toJSONFilter handleCmds
    toJSONFilter insertDiagrams 
    toJSONFilter insertDot 
    toJSONFilter copyImagesLocally

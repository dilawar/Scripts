#!/usr/bin/env runhaskell
import Text.Pandoc.JSON

import System.Process

import Control.Concurrent
import Data.List

main :: IO ()
main = toJSONFilter insertDot

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
    return $ Para $ [Image [Str "test"] (outFile,"test title")]

insertDot (CodeBlock (ident, ["showDotCode"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    let ext = getFileExtension outFile

    writeFile "tmp.dot" code
    system $ "dot tmp.dot -T" ++ ext ++ " > " ++ outFile
    return $ Div ("",[],[]) [(CodeBlock (ident, ["haskell"] , attrs) code)
                            ,Para $ [Image [Str "test"] (outFile,"test title")]
                            ]
insertDot x = return x


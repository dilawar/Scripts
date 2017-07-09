#!/usr/bin/env runhaskell
import Text.Pandoc.JSON

import System.Process

import Control.Concurrent
import Data.List

main :: IO ()
main = toJSONFilter insertDiagrams

insertDiagrams :: Block -> IO Block
insertDiagrams (CodeBlock (ident, ["diagram"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    writeFile "tmp.hs" code
    system "ghc -O2 tmp.hs -o tmp -rtsopts > /dev/null"
    system "./tmp +RTS -K100M > /dev/null"
    return $ Para $ [Image [Str "test"] (outFile,"test title")]
insertDiagrams (CodeBlock (ident, ["diagram","haskell"] , attrs) code) = do
    let Just (_,outFile) = find (\(key, value) -> key == "outFile") attrs
    writeFile "tmp.hs" code
    system "ghc -O2 tmp.hs -o tmp -rtsopts > /dev/null"
    system "./tmp +RTS -K100M > /dev/null"
    return $ Div ("",[],[]) [(CodeBlock (ident, ["haskell"] , attrs) code)
                            ,Para $ [Image [Str "test"] (outFile,"test title")]
                            ]
insertDiagrams x = return x


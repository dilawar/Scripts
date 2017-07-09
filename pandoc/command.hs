#!/usr/bin/env runhaskell
import Text.Pandoc.JSON

import System.Process

import Data.Monoid
import Data.List (find)

main :: IO ()
main = toJSONFilter handleCmds

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


#!/usr/bin/env runghc
{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import System.Process
import System.Posix.Temp
import System.IO
import Data.Maybe
import Text.Regex

includePlot :: Block -> IO Block
includePlot (CodeBlock (id, cls, attrs) code)
  | "gnuplot" `elem` cls
  = do
  (path, hndl) <- mkstemp "/tmp/pandoc-z-gnuplot"
  hPutStr hndl code
  hClose hndl
  callCommand $ (subRegex (mkRegex "<!>") cmd (path ++ ".png")) ++ " " ++ path
  return $ Plain [Image ("", ["graphviz"], [("", "")]) [Str txt] (path ++ ".png", title)]
  where
    cmd = fromMaybe "gnuplot -e \"set term png; set output '<!>'\"" $ lookup "plotcmd" attrs
    title = fromMaybe "" $ lookup "title" attrs
    txt = fromMaybe "" $ lookup "plotcmd" attrs

includePlot x = return x

main :: IO ()
main = toJSONFilter includePlot

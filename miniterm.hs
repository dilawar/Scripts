#!/usr/bin/runhaskell

import System.IO
import System.Environment
import System.Hardware.Serialport
import Control.Monad 
import Control.Monad.Fix 
import qualified Data.ByteString.Char8 as B
import System.Console.GetOpt
import System.Exit

helpMsg = "USAGE: ./miniterm.hs /dev/ttyName baudrate"

eol = B.singleton '\n'

fetchLine s = do 
    c <- recv s 1
    if c == eol then
        return B.empty
    else do
        vs <- fetchLine s
        return $! B.append c vs

keepReading h = do 
    fetchLine h >>= print 
    isDataAvailable <- hReady stdin
    if isDataAvailable  
      then do
        char <- hGetChar stdin
        if char == '\^]' 
          then
            exitSuccess
          else 
            send h $ B.singleton char
      else do
        return 0

speed :: String -> CommSpeed 
speed x | x == "9600" = CS9600 
        | x == "19200" = CS19200 
        | x == "38400" = CS38400 
        | x == "57600" = CS57600 
        | x == "115200" = CS115200 
        | otherwise = CS9600

main = do
    args <- getArgs
    -- Make sure that input is available without CR 
    hSetBuffering stdin NoBuffering
    s <- openSerial (args!!0) defaultSerialSettings { commSpeed = speed (args!!1) }
    mapM (\x -> keepReading s) [1..]
    closeSerial s
    putStrLn "All done"



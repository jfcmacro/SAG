module Main(main) where

import CFG
import AG
import AGParser
import DirectDependencies(dp,idp)
import System(getArgs)
import IO(stderr,hPutStr)

process :: FilePath -> IO ()
process f = do
  s <- readFile f
  ag <- parseIOAG (scanAG f s)
  let pr = dp (sem ag)
  print "printing the set of DP(p)"
  print pr
  let iswd = isCFGWellDefined (cfg ag)
  either (print) (print) iswd
  let occurPR_consits = occurPR_p (inhSynLocAttrTypes_X (attrs ag))
                        (prods (cfg ag))
                        "ConsIts"
  print "printing the occurrence set of ConsIts"
  print occurPR_consits
  print "ipd"
  let ipd' = idp ag
  print ipd'

use :: String -> String
use msg = "Usage: sag <filepath> " ++ "\n" ++ msg

usage :: String -> IO ()
usage msg = do
  hPutStr stderr (use msg)

-- | The 'main' function
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
   then usage ""
   else do let ifile = args !! 0
           process ifile `catch` handle
           return ()
               where handle e = hPutStr stderr (show e)

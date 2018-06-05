module Main where

import           Lib

main :: IO ()
main = do print "hi"
          demo

demo = do bigtext <- readFile "WizardOfOz.txt"
          print $ take 50 bigtext

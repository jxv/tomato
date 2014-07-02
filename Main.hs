module Main where

import Tomato.Core
import Tomato.Ui hiding (main)

import qualified Tomato.Ui as Ui (main)

main :: IO ()
main = Ui.main


module Main where

import qualified Drivers.Cli as CliDriver (main)

-- | Main project entrypoint.
main :: IO ()
main = CliDriver.main

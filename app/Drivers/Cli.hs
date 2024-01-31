{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command-line entrypoint.
module Drivers.Cli (main) where

import Import
import Jangle (run)
import Options.Applicative.Simple
import qualified Paths_jangle
import RIO.Process
import RIO.Text (pack)
import System.Environment.Blank (getEnv)

-- | Runs a command-line interface to the program, parsing all relevant
-- | command-line arguments.
main :: IO ()
main = do
  (cliOptions, ()) <-
    simpleOptions
      $(simpleVersion Paths_jangle.version)
      "Advent of Code problem runner"
      ( unwords
          [ "Jangle is a problem runner, and template project for Advent of",
            "Code, compatible with all \"years\"."
          ]
      )
      ( Options
          <$> switch
            ( long "verbose"
                <> help "Verbose logger output"
            )
          <*> option
            auto
            ( long "year"
                <> help "Problem year to run"
                <> metavar "YEAR"
            )
          <*> option
            auto
            ( long "day"
                <> help "Problem day to run"
                <> metavar "DAY"
            )
          <*> option
            auto
            ( long "part"
                <> help "Problem part to run"
                <> metavar "PART"
            )
      )
      empty
  authToken <- fmap pack <$> getEnv "AUTH_TOKEN"
  let options = cliOptions authToken
  logOptions <- logOptionsHandle stderr $ optionsVerbose options
  processContext <- mkDefaultProcessContext
  withLogFunc logOptions $ \logFunc ->
    let app =
          App
            { appOptions = options,
              appLogFunc = logFunc,
              appProcessContext = processContext,
              appProblemId =
                ProblemId
                  { problemIdYear = optionsYear options,
                    problemIdDay = optionsDay options,
                    problemIdPart = optionsPart options
                  }
            }
     in runRIO app run

-- | Project-wide type definitions.
-- |
-- | Note that this is re-expored from `Import`, so these types should truly be
-- | considered project-wide.
module Types
  ( App (..),
    Options (..),
    ProblemId (..),
  )
where

import RIO
import RIO.Process

-- | App-wide context for this app.
-- | This is usually used as the `RIO` reader parameter for this app.
data App = App
  { appOptions :: !Options,
    appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appProblemId :: !ProblemId
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

-- | Options for running this app via its main entrypoint.
data Options = Options
  { -- | If `True`, log messages will be more verbose.
    optionsVerbose :: !Bool,
    -- | The year of the problem to run.
    optionsYear :: !Int,
    -- | The problem day to run.
    optionsDay :: !Int,
    -- | The problem part to run.
    optionsPart :: !Int,
    -- | The token used to authenticate with Advent of Code services.
    optionsAuthToken :: !(Maybe Text)
  }

-- | Identifies an Advent of Code problem.
data ProblemId = ProblemId
  { problemIdYear :: !Int,
    problemIdDay :: !Int,
    problemIdPart :: !Int
  }

instance Display ProblemId where
  display (ProblemId year day part) =
    "{ year = "
      <> display year
      <> ", day = "
      <> display day
      <> ", part = "
      <> display part
      <> " }"

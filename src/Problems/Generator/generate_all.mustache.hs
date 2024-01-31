module Problems.All (Error(..), runProblem) where

import Import
{{#modules}}
import qualified Problems.{{name}} as {{name}}
{{/modules}}

data Error =
  -- | Returned when the problem could not be found.
  UnknownProblem ProblemId
  -- | Returned when the part number is invalid.
  | InvalidPart Int

instance Display Error where
  display (UnknownProblem id) = "Problem ID not registered: " <> display id
  display (InvalidPart x) = "Invalid problem part: " <> display x

runProblem :: Text -> ProblemId -> Either Error Text
runProblem input id
{{#modules}}
  | (problemIdYear id) == {{year}}
    && (problemIdDay id) == {{day}} = do
      runner <- case problemIdPart id of
                  1 -> Right {{name}}.runPartOne
                  2 -> Right {{name}}.runPartTwo
                  n -> Left $ InvalidPart n
      Right $ runner input
{{/modules}}
  | otherwise = Left $ UnknownProblem id

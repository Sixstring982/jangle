module Jangle (run) where

import Import
import Network.HTTP.Client (Request (requestHeaders), Response (responseBody), httpLbs, newManager, parseRequest, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (Status))
import Problems.All (runProblem)
import RIO.ByteString (toStrict, unpack)
import RIO.ByteString.Lazy qualified as L
import RIO.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import RIO.FilePath ((</>))
import RIO.Text (intercalate, pack)
import Text.Printf (printf)
import System.IO (putStrLn)
import Prelude (print)

-- | Main library entrypoint.
run :: RIO App ()
run = do
  id <- asks appProblemId
  input <- readInput
  logDebug $ "Running problem: " <> display id
  case runProblem input id of
    Left m ->  logError $ "Error running problem: " <> display m
    Right out -> liftIO $ print out

-- | Reads the input for the configured ProblemId from disk if available,
-- | or fetches it from adventofcode.com if not, then writes the input to disk
-- | for later.
readInput :: RIO App Text
readInput = do
  filename <- toFilename <$> asks appProblemId
  fileExists <- doesFileExist filename
  if fileExists
    then do
      logDebug $ "Reading cached input from disk: " <> displayShow filename
      readFileUtf8 filename
    else fetchAndCacheInput

fetchAndCacheInput :: RIO App Text
fetchAndCacheInput = do
  filename <- toFilename <$> asks appProblemId
  year <- problemIdYear <$> asks appProblemId
  input <- fetchInput
  currentDirectory <- getCurrentDirectory
  () <- ensureDirectoryExists $ currentDirectory </> "inputs"
  () <- ensureDirectoryExists $ currentDirectory </> "inputs" </> show year
  () <- writeFileUtf8 filename input
  return input

-- | Contacts Advent of Code's servers in order to fetch the problem input for
-- | the configured ProblemId.
fetchInput :: RIO App Text
fetchInput = do
  (ProblemId year day part) <- asks appProblemId
  let url = printf "GET https://adventofcode.com/%d/day/%d/input" year day
  logDebug $ "Fetching problem input: " <> displayShow url
  authToken <- optionsAuthToken <$> asks appOptions
  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- parseRequest url
  let request = case authToken of
        Nothing -> initialRequest
        Just authToken ->
          initialRequest
            { requestHeaders =
                [ ("Cookie", "session=" <> encodeUtf8 authToken)
                ]
            }
  response <- liftIO $ httpLbs request manager
  case responseStatus response of
    (Status 200 _) -> return $ decodeUtf8Lenient $ toStrict $ responseBody response
    (Status n msg) -> do
      logError $ "Failed to fetch problem input: " <> display n <> ": " <> displayShow msg
      error "Exiting."

class ToFilename a where
  toFilename :: a -> FilePath

instance ToFilename ProblemId where
  toFilename (ProblemId year day _) = printf "./inputs/%d/%d.txt" year day

ensureDirectoryExists :: FilePath -> RIO App ()
ensureDirectoryExists dir = do
  exists <- doesDirectoryExist dir
  if exists
    then return ()
    else createDirectory dir

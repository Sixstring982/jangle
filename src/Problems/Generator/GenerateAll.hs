#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate, isPrefixOf)
import RIO (Text)
import RIO.Text (pack, split, unpack)
import RIO.Vector (fromList)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.IO (IOMode (..), hClose, hFlush, openFile)
import Text.Mustache (Template, ToMustache (..), compileTemplate, object, substitute, (~>))
import Text.Mustache.Types (Value (..))
import Prelude

main :: IO ()
main = do
  [_, _, filename] <- getArgs
  -- Compute module names, by reading parent directory
  files <- getDirectoryContents "./src/Problems"
  let files' = filter (\x -> "Problem" `isPrefixOf` x) files
  let modules = pack . takeWhile (/= '.') <$> files'
  -- Compile and render mustache template
  template <- readTemplate
  let arguments = toTemplateArguments modules
  let renderedTemplate = substitute template arguments
  writeFile filename (unpack renderedTemplate)
  return ()

readTemplate :: IO Template
readTemplate = do
  templateSource <- pack <$> readFile "./src/Problems/Generator/generate_all.mustache.hs"
  case compileTemplate "generate_all.mustache.hs" templateSource of
    Left e -> error $ show e
    Right t -> return t

data Module = Module
  { moduleName :: !Text,
    moduleYear :: !Text,
    moduleDay :: !Text
  }

instance ToMustache Module where
  toMustache x =
    object
      [ "name" ~> moduleName x,
        "year" ~> moduleYear x,
        "day" ~> moduleDay x
      ]

toModule :: Text -> Module
toModule name =
  let parts = split (== '_') name
      year = parts !! 1
      day = parts !! 2
   in Module name year day

newtype TemplateArguments = TemplateArguments
  { templateArgumentsModules :: [Module]
  }

instance ToMustache TemplateArguments where
  toMustache x =
    object
      [ "modules"
          ~> Array (fromList $ toMustache <$> templateArgumentsModules x)
      ]

toTemplateArguments :: [Text] -> TemplateArguments
toTemplateArguments = TemplateArguments . fmap toModule

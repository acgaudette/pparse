module Options where

import System.Console.GetOpt

data Options = Options {
    optInput :: String
  , optOutput :: String
  , optNamespace :: Maybe String
  , optContainer :: Maybe String
  , optIncludes :: [String]
  , optIgnores :: [String]
  , optCherries :: [String]
}

defaultOptions = Options {
    optInput = "in.js"
  , optOutput = "out.cs"
  , optNamespace = Nothing
  , optContainer = Nothing
  , optIncludes = []
  , optIgnores = []
  , optCherries = []
}

options = [
    Option ['i'] ["input"] (
      ReqArg (\ path options -> options { optInput = path })
      "path"
    ) "file for reading"
  , Option ['o'] ["output"] (
      ReqArg (\ path options -> options { optOutput = path })
      "path"
    ) "file for writing"
  , Option ['n'] ["namespace"] (
      ReqArg (\ name options -> options { optNamespace = Just name })
      "namespace"
    ) "namespace for output header"
  , Option ['c'] ["container"] (
      ReqArg (\ name options -> options { optContainer = Just name })
      "container"
    ) "container for output class"
  , Option ['l'] ["include"] (
      ReqArg (\ include options ->
        options { optIncludes = optIncludes options ++ [include] })
      "include"
    ) "using directive for output header"
  , Option ['I'] ["ignore"] (
      ReqArg (\ name options ->
        options { optIgnores = optIgnores options ++ [name] })
      "name"
    ) "property name to ignore"
  , Option ['C'] ["cherry"] (
      ReqArg (\ name options ->
        options { optCherries = optCherries options ++ [name] })
      "name"
    ) "container name to parse"
  ]

getOptions args =
  case getOpt Permute options args of
    (o, _, []) -> return (foldl (flip id) defaultOptions o)
    (_, _, e ) -> ioError (userError (concat e ++ usageInfo message options))
  where message = "Usage: pparse [option...]"

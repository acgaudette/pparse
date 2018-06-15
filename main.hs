import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char

data Options = Options {
    optInput :: String
  , optOutput :: String
  , optNamespace :: Maybe String
  , optContainer :: Maybe String
  , optIgnores :: [String]
  , optCherries :: [String]
}

defaultOptions = Options {
    optInput = "in.js"
  , optOutput = "out.cs"
  , optNamespace = Nothing
  , optContainer = Nothing
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

main = do
  args <- getArgs
  opts <- getOptions args

  let Options {
      optInput = inPath
    , optOutput = outPath
  } = opts

  input <- openFile inPath ReadMode
  output <- openFile outPath WriteMode

  contents <- hGetContents input
  hPutStr output (parse contents opts)

  hClose input
  hClose output

data Token = Container String
           | Field String
           | Value String String String
           | Close

parse text opts = header opts ++ generate (scan text) opts ++ footer

-- Scanning --

scan text =
  if null text then []
  else case head text of
    '\'' -> parseName remainder
    '[' -> parseValue remainder
    '}' -> parseClose remainder
    _ -> scan remainder -- Skip
  where remainder = tail text

parseName text =
  [fst result] ++ scan (snd result)
    where result = detect $ scanName text

detect scanned =
  case head text of
    '{' -> (Container input, remainder)
    ' ' -> detect (input, remainder) -- Skip
    ':' -> detect (input, remainder) -- Skip
    _ -> (Field input, text) -- Reuse char in scan
  where input = fst scanned
        text = snd scanned
        remainder = tail text

scanName text =
  if char == '\''
    then ("", tail text)
    else (char : fst result, snd result)
      where char = head text
            result = scanName $ tail text

parseValue text =
  [Value (numbers !! 0) (numbers !! 1) (numbers !! 2)] ++ scan (snd result)
    where result = scanValue text
          numbers = fst result

scanValue text =
  if head text == ']'
    then ([], tail text)
  else parseNumber text

parseNumber text =
  ([fst result] ++ fst next, snd next)
    where result = scanNumber text
          next = scanValue $ snd result

scanNumber text =
  case char of
    ',' -> ("", remainder)
    ']' -> ("", text) -- Reuse char in scanValue
    ' ' -> scanNumber remainder
    _ -> (char : fst result, snd result)
  where char = head text
        remainder = tail text
        result = scanNumber $ remainder

parseClose text = [Close] ++ scan text

-- Generation --

generate tokens opts =
  if null tokens then ""
  else case head tokens of
    Container name -> genClass name remainder opts
    _ -> generate remainder opts -- Skip
  where remainder = tail tokens

genClass name remainder opts =
  if elem name (map munge (optCherries opts))
    then
      mkClass 2 (toPascal name)
      ++ fst fields
      ++ generate (snd fields) opts
    else "" ++ generate remainder opts
      where fields = genFields remainder (optIgnores opts)

genFields tokens ignores =
  case head tokens of
    Field name -> (genField name ignores ++ fst result, snd result)
    Value def min max -> ((mkField def min max) ++ fst result, snd result)
    Close -> (mkClose, tail tokens)
  where result = genFields (tail tokens) ignores

genField name ignores =
  if elem name (map munge ignores) then "" -- Skip ignored names
  else mkFloat ++ toCamel name

-- Replace underscore with space
munge ignore =
  if null ignore then ""
  else if char == '_' then ' ' : munge remainder
  else char : munge remainder
    where char = head ignore
          remainder = tail ignore

-- No case to Pascal case conversion
toPascal name = toUpper (head name) : toCamel (tail name)

-- No case to camel case conversion
toCamel name =
  if null name
    then ""
  else if char == ' '
    then toUpper (head remainder) : toCamel (tail remainder)
  else char : toCamel remainder
    where char = head name
          remainder = tail name

-- C# string functions --

endl = "\n"
tab count = concat $ replicate count "  "

header opts =
  "/* Generated by acgaudette/pparse; do not modify */"
  ++ endl ++ endl
  ++ case optNamespace opts of
    Just name -> "namespace " ++ name ++ " "
    Nothing -> ""
  ++ "{" ++ endl
  ++ mkClass 1 (
    case optContainer opts of
      Just name -> name
      Nothing -> "Properties"
    )

footer = tab 1 ++ "}" ++ endl ++ "}" ++ endl

mkClass indent name = endl
  ++ tab indent ++ "[System.Serializable]" ++ endl
  ++ tab indent ++ "public class " ++ name ++ " {" ++ endl

mkRange min max = tab 3
  ++ "[Range(" ++ min ++ "f, " ++ max ++ "f)] "

mkField def min max =
  " = " ++ def ++ "f;"
  ++ " // (" ++ min ++ ", " ++ max ++ ")"
  ++ endl
mkFloat = "public float "

mkClose = tab 2 ++ "}" ++ endl

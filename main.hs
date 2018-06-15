import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char

data Options = Options {
    optInput :: String
  , optOutput :: String
  , optNamespace :: Maybe String
  , optIgnores :: [String]
  , optCherries :: [String]
}

defaultOptions = Options {
    optInput = "in.js"
  , optOutput = "out.cs"
  , optNamespace = Nothing
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
    optInput = inPath,
    optOutput = outPath,
    optNamespace = namespace,
    optIgnores = ignores,
    optCherries = cherries
  } = opts

  input <- openFile inPath ReadMode
  output <- openFile outPath WriteMode

  contents <- hGetContents input
  hPutStr output (parse contents namespace ignores cherries)

  hClose input
  hClose output

data Token = Name String -- Property or container identifier
           | Value String String String -- Property value
           | Close -- Closing brace

parse text namespace ignores cherries =
  header namespace ++ generate (scan text) ignores cherries ++ footer

-- Scanning --

scan text =
  if null text
    then []
  else if char == '\''
    then parseName remainder
  else if char == '['
    then parseValue remainder
  else if char == '}'
    then parseClose remainder
  else scan remainder -- Skip
    where char = head text
          remainder = tail text

parseName text =
  [Name (fst result)] ++ scan (snd result)
    where result = scanName text

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
  if char == ','
    then ("", remainder)
  else if char == ']'
    then ("", text) -- Go straight back to ScanValue
  else if char == ' ' -- Ignore spaces
    then scanNumber remainder
  else (char : fst result, snd result)
    where char = head text
          remainder = tail text
          result = scanNumber $ remainder

parseClose text = [Close] ++ scan text

-- Generation --

generate tokens ignores cherries =
  if null tokens
    then ""
    else case head tokens of
      Name name -> genClass name (tail tokens) ignores cherries
      otherwise -> generate (tail tokens) ignores cherries -- Skip

genClass name remainder ignores cherries =
  if elem name (map munge cherries)
    then
      mkClass 2 (toCamel name)
      ++ fst fields
      ++ generate (snd fields) ignores cherries
    else "" ++ generate remainder ignores cherries
      where fields = genFields remainder ignores

genFields tokens ignores =
  case head tokens of
    Name name -> (genName name ignores ++ fst result, snd result)
      where result = genFields (tail tokens) ignores
    Value def min max -> ((mkField def min max) ++ fst result, snd result)
      where result = genFields (tail tokens) ignores
    Close -> (mkClose, tail tokens)

genName name ignores =
  if elem name (map munge ignores) then "" -- Skip ignored names
  else mkFloat ++ toCamel name

-- Replace underscore with space
munge ignore =
  if null ignore then ""
  else if char == '_' then ' ' : munge remainder
  else char : munge remainder
    where char = head ignore
          remainder = tail ignore

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

header namespace =
  case namespace of
    Just name -> "namespace " ++ name ++ " "
    Nothing -> ""
  ++ "{" ++ endl
  ++ mkClass 1 "Properties"

footer = tab 1 ++ "}" ++ endl ++ "}" ++ endl

mkClass indent name = tab indent ++ "[System.Serializable]" ++ endl
  ++ tab indent ++ "public class " ++ name ++ " {" ++ endl

mkFloat = tab 3 ++ "public float "

mkField def min max =
  " = " ++ def ++ "f;"
  ++ " // (" ++ min ++ ", " ++ max ++ ")"
  ++ endl

mkClose = tab 2 ++ "}" ++ endl

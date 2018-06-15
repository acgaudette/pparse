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

main = do
  input <- openFile inPath ReadMode
  output <- openFile outPath WriteMode

  contents <- hGetContents input
  hPutStr output (parse contents)

  hClose input
  hClose output

data Token = Name String -- Property or container identifier
           | Value String String String -- Property value
           | Close -- Closing brace

parse text = header ++ generate (scan text) ++ footer

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

generate tokens =
  if null tokens
    then ""
    else case head tokens of
      Name name -> genClass name (tail tokens)
      otherwise -> generate (tail tokens) -- Skip

genClass name remainder =
  if name == "upper body" || name == "lower body"
    then mkClass 2 (toCamel name) ++ fst fields ++ generate (snd fields)
    else "" ++ generate remainder
      where fields = genFields remainder

genFields tokens =
  case head tokens of
    Name name -> (genName name ++ fst result, snd result)
      where result = genFields (tail tokens)
    Value def min max -> ((mkField def min max) ++ fst result, snd result)
      where result = genFields (tail tokens)
    Close -> (mkClose, tail tokens)

genName name =
  if name == "knees forward" then "" -- Ignore (FIXME)
  else mkFloat ++ toCamel name

-- No case to camel case conversion
toCamel name =
  if null name
    then ""
  else if head name == ' '
    then toUpper (head remainder) : toCamel (tail remainder)
  else head name : toCamel remainder
    where remainder = tail name

-- C# string functions --

endl = "\n"
tab count = concat $ replicate count "  "

header = "namespace AutoToon.Character {" ++ endl
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

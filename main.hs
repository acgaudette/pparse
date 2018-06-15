import System.IO
import Data.Char

inPath = "input.js"
outPath = "out.cs"

main = do
  input <- openFile inPath ReadMode
  output <- openFile outPath WriteMode

  contents <- hGetContents input
  hPutStr output (parse contents)

  hClose input
  hClose output

data Token = Name String
           | Value String String String
           | Close

parse text =
  header ++ generate (scan text) ++ footer

scan text =
  if null text
    then []
  else if char == '\''
    then parseName remainder
  else if char == '['
    then parseValue remainder
  else if char == '}'
    then parseClose remainder
  else scan remainder
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
    then ("", text)
  else if char == ' '
    then scanNumber remainder
  else (char : fst result, snd result)
    where char = head text
          remainder = tail text
          result = scanNumber $ remainder

parseClose text = [Close] ++ scan text

generate tokens =
  if null tokens
    then ""
    else case head tokens of
      Name name -> genClass name (tail tokens)
      Value _ _ _ -> generate (tail tokens) -- Ignore
      Close -> generate (tail tokens) -- Ignore

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
  if name == "knees forward"
    then ""
  else mkFloat ++ toCamel name

toCamel name =
  if null name
    then ""
  else if head name == ' '
    then toUpper (head remainder) : toCamel (tail remainder)
  else head name : toCamel remainder
    where remainder = tail name

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

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
    then parseName (scanName remainder)
  else if char == '['
    then parseValue (scanValue remainder)
  else if char == '}'
    then [Close] ++ scan remainder
  else scan remainder
    where char = head text
          remainder = tail text

parseName result =
  [Name (fst result)] ++ scan (snd result)
scanName text =
  if char == '\''
    then ("", tail text)
    else (char : fst result, snd result)
      where char = head text
            result = scanName $ tail text

parseValue result =
  [Value (numbers !! 0) (numbers !! 1) (numbers !! 2)] ++ scan (snd result)
    where numbers = fst result
scanValue text =
  if head text == ']'
    then ([], tail text)
  else parseNumber (scanNumber text)

parseNumber result =
  ([fst result] ++ fst new, snd new)
    where new = scanValue $ snd result
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

generate tokens =
  if null tokens
    then ""
    else case head tokens of
      Name name -> genClass name (tail tokens)
      Value _ _ _ -> generate (tail tokens) -- Ignore
      Close -> generate (tail tokens) -- Ignore

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

mkFloat = tab 3 ++ "public float "

mkClass indent name = tab indent ++ "[System.Serializable]" ++ endl
  ++ tab indent ++ "public class " ++ name ++ " {" ++ endl

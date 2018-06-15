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

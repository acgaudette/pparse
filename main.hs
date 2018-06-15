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

endl = "\n"
tab count = concat $ replicate count "  "

header = "namespace AutoToon.Character {" ++ endl
  ++ mkClass 1 "Properties"
footer = tab 1 ++ "}" ++ endl ++ "}" ++ endl

mkFloat = tab 3 ++ "public float "

mkClass indent name = tab indent ++ "[System.Serializable]" ++ endl
  ++ tab indent ++ "public class " ++ name ++ " {" ++ endl

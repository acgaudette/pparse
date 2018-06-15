import System.IO
import Data.List
import Data.Char

main = do
  inFile <- openFile "input.js" ReadMode
  outFile <- openFile "out.cs" WriteMode

  hPutStrLn outFile header
  search inFile outFile
  hPutStrLn outFile footer

  hClose inFile
  hClose outFile

-- Search for objects to parse, line by line
search inFile outFile = do
  ok <- hIsEOF inFile
  if ok
    then return ()
    else do
      line <- hGetLine inFile
      if null $ check line
        then search inFile outFile
        else do
          hPutStrLn outFile $ struct $ check line
          readProps inFile outFile

-- Look for desired object title keywords
check line =
  if isSubsequenceOf "'upper body'" line
    then "UpperBody"
  else if isSubsequenceOf "'lower body'" line
    then "LowerBody"
  else ""

-- Read and write properties, line by line
readProps inFile outFile = do
  line <- hGetLine inFile
  if isSubsequenceOf "}" line
    -- We're done, go back to searching
    then do
      hPutStrLn outFile "}"
      search inFile outFile
    -- Read and write property
    else do
      hPutStrLn outFile $ readProp line
      readProps inFile outFile

readProp line =
  if head line == '\''
    then float ++ mkProp (tail line)
    else readProp $ tail line

-- Convert engine property to C#
mkProp prop =
  if head prop == '\''
    then ";"
  else if head prop == ' '
    then toUpper (head (tail prop)) : mkProp (tail (tail prop))
  else head prop : mkProp (tail prop)

-- String literals

header = "namespace AutoToon.Character {\n"
  ++ struct "InitProperties"
tab count = concat $ replicate count "  "
endl = "\n"

struct name = "[System.Serializable]\n"
  ++ "public struct " ++ name ++ " {"

float = "public float "

footer = "}\n}"

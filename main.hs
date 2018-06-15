import System.IO
import Data.List
import Data.Char

-- String literals

header = "namespace AutoToon.Character {\n"
  ++ struct "InitProperties"

struct name = "[System.Serializable]\n"
  ++ "public struct " ++ name ++ " {"

float = "public float "

footer = "}\n}"

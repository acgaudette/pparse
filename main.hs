import System.IO
import System.Environment

import Options
import Parser

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

-- | Command-Line driver for compiling Verse documents.

import Control.Applicative
import Control.Monad
import System.Environment (getArgs)
import System.Exit

import Language.Verse.Parser

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (putStrLn "Provide an input file." >> exitWith (ExitFailure 1))
    let inFile = head args
    fileContents <- readFile inFile
    print $ runParser document inFile fileContents

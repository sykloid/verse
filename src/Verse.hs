-- | Command-Line driver for compiling Verse documents.

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import System.IO
import System.Environment (getArgs)
import System.Exit

import Language.Verse.Parser
import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import Language.Verse.Transform.Bold
import Language.Verse.Transform.Italics

import Language.Verse.Transform.Chess
import Language.Verse.Transform.Code
import Language.Verse.Transform.Music

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (putStrLn "Provide an input file." >> exitWith (ExitFailure 1))
    let inFile = head args
    fileContents <- readFile inFile
    let Right x = runParser document inFile fileContents
    let transforms = M.fromList [
                ("bold", TransformContext BoldTC)
            ,   ("italics", TransformContext ItalicsTC)
            ,   ("code", TransformContext $ CodeTC "pygmentize")
            ,   ("music", TransformContext $ MusicTC False "vexflow.js" "jquery.js")
            ,   ("chess", TransformContext $ ChessTC 0 "chessboard.js")
            ]
    y <- runRenderer transforms HtmlRenderContext . renderD $ x
    putStrLn $ serialize y

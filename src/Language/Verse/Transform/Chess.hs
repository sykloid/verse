{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Verse.Transform.Chess where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid
import Data.String

import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans

data ChessTC = ChessTC { chessCount :: Int, chessBoardPath :: String } deriving Show

scriptInclude :: String -> Rendering HtmlRenderContext
scriptInclude path = H.script ! A.src (fromString path) $ mempty

instance Transform HtmlRenderContext ChessTC where
    renderTransform cc s = do
        lift . lift . put $ TransformContext (cc { chessCount = chessCount cc + 1 })
        return $ chessInclude <> (H.script $ H.toHtml $ unwords [playFn, eventListener]) <> (H.canvas ! A.id (fromString bdName) $ mempty)
      where
        fnName = "play" ++ show (chessCount cc)
        bdName = "board" ++ show (chessCount cc)
        chessInclude =
            if chessCount cc < 1
                then scriptInclude (chessBoardPath cc)
                else mempty

        moveList =
            if null s
                then ""
                else "game.move(\"" ++ s ++ "\");"

        gameDef = "game = Chessboard.newGame();"

        drawCmd = "game.draw(\"" ++ bdName ++ "\");"

        eventListener = "window.addEventListener('load', " ++ fnName ++ ", false);"

        playFn = "function " ++ fnName ++ "() {" ++ gameDef ++ "\n" ++ moveList ++ "\n" ++ drawCmd ++ "}"
        {- H.img ! A.src ( -}
            {- fromString $ "http://www.eddins.net/steve/chess/ChessImager/ChessImager.php?fen=" ++ s ++ "/" -}
        {- ) -}

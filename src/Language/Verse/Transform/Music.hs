{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Verse.Transform.Music where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as P

import Data.String
import Data.Monoid
import Data.List
import Data.List.Split

import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans

data MusicTC = MusicTC { included :: Bool, vexFlowPath :: String, jQueryPath :: String} deriving Show

data Note = Note { pitch :: String, octave :: String, duration :: String }

scriptInclude :: String -> Rendering HtmlRenderContext
scriptInclude path = H.script ! A.src (fromString path) $ mempty

canvasSetup :: String -> String -> Rendering HtmlRenderContext
canvasSetup width height = H.canvas ! A.width (fromString width) ! A.height (fromString height) $ mempty

parseNotes :: String -> [Note]
parseNotes s = [Note p o d | [p, o, d] <- chunksOf 3 $ words s]

instance Transform HtmlRenderContext MusicTC where
    renderTransform bp s = do
        lift . lift . put $ (TransformContext $ bp { included = False })
        return $
            (if not $ included bp
                then scriptInclude (vexFlowPath bp) <> scriptInclude (jQueryPath bp)
                else mempty)
            <> canvasSetup "600" "200" <> (H.script $ H.toHtml (vexFlowPrelude ++ vexFlowNotes (parseNotes s) ++ vexFlowPostlude (ticks $ parseNotes s)))

ticks :: [Note] -> Int
ticks [] = 0
ticks (Note _ _ d:ns) =
    case head d of
        'q' -> 1
        'h' -> 2
        'w' -> 4
        _   -> 0
    + ticks ns

vexFlowPrelude :: String
vexFlowPrelude = unlines [
        "var canvas = $(\"canvas\")[0];"
    ,   "var renderer = new Vex.Flow.Renderer(canvas, Vex.Flow.Renderer.Backends.CANVAS);"
    ,   "var ctx = renderer.getContext();"
    ,   "var stave = new Vex.Flow.Stave(10, 0, 500);"
    ,   "stave.addClef(\"treble\").setContext(ctx).draw();"
    ]

vexFlowNotes :: [Note] -> String
vexFlowNotes ns = "var notes = [" ++ intercalate ", " [
    "new Vex.Flow.StaveNote({ keys: [\""
        ++ pitch n ++ "/" ++ octave n ++ "\"], duration: \"" ++ duration n ++
    "\" })" | n <- ns] ++ "];"

vexFlowPostlude :: Show a => a -> String
vexFlowPostlude t = unlines [
        "var voice = new Vex.Flow.Voice({ num_beats: " ++ show t ++ ", beat_value: 4, resolution: Vex.Flow.RESOLUTION });"
    ,   "voice.addTickables(notes);"
    ,   "var formatter = new Vex.Flow.Formatter().joinVoices([voice]).format([voice], 500);"
    ,   "voice.draw(ctx, stave);"
    ]

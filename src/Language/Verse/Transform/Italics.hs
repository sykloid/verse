{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Verse.Transform.Italics where

import Text.Blaze.Html5 as H

import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans

data ItalicsTC = ItalicsTC deriving (Show)

instance Transform HtmlRenderContext ItalicsTC where
    renderTransform _ s = return (H.em $ H.toHtml s)

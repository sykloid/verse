{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Transform.Bold where

import Text.Blaze.Html5 as H

import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans

data BoldTC = BoldTC deriving (Show)

instance Transform HtmlRenderContext BoldTC where
    renderTransform _ s = return (H.strong $ H.toHtml s)

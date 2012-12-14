{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Verse.Transform.Code where

import Text.Blaze.Html5 as H

import Language.Verse.Renderer
import Language.Verse.Renderer.Html

import System.IO
import System.Process
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans

data CodeTC = CodeTC { pygmentsCommand :: String } deriving (Show)

instance Transform HtmlRenderContext CodeTC where
    renderTransform (CodeTC { pygmentsCommand }) s = do
        let (language, code) = break (== ':') s
        parsedCode <- liftIO $
            readProcess pygmentsCommand ["-f", "html", "-l", language, "-O", "noclasses"] (tail code)
        return $ H.preEscapedToHtml parsedCode

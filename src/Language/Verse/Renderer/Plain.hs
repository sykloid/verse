{-# LANGUAGE TypeFamilies #-}

module Language.Verse.Renderer.Plain where

import Language.Verse.AST
import Language.Verse.Renderer

data PlainContext = PlainContext

instance RenderContext PlainContext where
    type Rendering PlainContext = String

    renderA _ (Document ls) = unlines $ map renderBlock ls
      where
        renderBlock (Paragraph is) = unwords $ map renderInline is

        renderInline (Content t) = t
        renderInline (Transform t _) = t

    renderP _ d = ""
    renderB _ d = ""

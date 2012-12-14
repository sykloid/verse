{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Verse.Renderer.Html where

import System.IO

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import Data.String

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as P

import Language.Verse.AST
import Language.Verse.Renderer
import Language.Verse.Transform

data HtmlRenderContext = HtmlRenderContext deriving Show

instance RenderContext HtmlRenderContext where
    type Rendering HtmlRenderContext = H.Html

    renderD (Document bs) = do
        liftIO $ hPrint stderr "Rendering Document in Html."
        blocks <- mapM renderB bs
        let head = H.head $ H.toHtml ""
        let body = H.body $ mconcat blocks
        return $ H.docTypeHtml $ head <> body

    renderP (Paragraph is) = do
        liftIO $ hPrint stderr "Rendering Paragraph."
        inlines <- mapM renderI is
        return $ H.p $ mconcat inlines

    renderC (Content s) = do
        liftIO $ hPrint stderr "Rendering Content."
        return $ H.toHtml s

    renderT (Transform t s) = do
        liftIO $ hPrint stderr "Rendering Transform"
        transforms <- vLift get
        let transform = M.lookup t transforms
        case transform of
            Nothing -> return $
                H.span ! A.class_ (fromString "error") $ H.toHtml $ "Unknown transform " ++ show t
            Just (TransformContext tn) -> do
                lift . lift $ put (TransformContext tn)
                result <- renderTransform tn s
                ns <- lift $ lift get
                modify (M.insert t ns)
                lift . lift . put $ TransformContext ()
                return result

serialize :: Rendering HtmlRenderContext -> String
serialize = P.renderHtml

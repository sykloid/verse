{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Verse.Renderer (
    RenderContext(..),
    RendererT,
    Renderer,
    runRenderer,
    VerseState,
    TransformState,
    Transform(..),
    TransformContext(..),
    vLift, rLift, tLift
) where

import Control.Monad.State
import qualified Data.Map as M

import Language.Verse.AST

class Transform r t where
    renderTransform :: t -> String -> Renderer r (Rendering r)

instance Transform r () where
    renderTransform = error "()"

data TransformContext r = forall t. Transform r t => TransformContext t

type RendererT v r t m a = StateT v (StateT r (StateT t m)) a

type TransformState = ()
type VerseState r = M.Map String (TransformContext r)

type Renderer r a = RendererT (VerseState r) r (TransformContext r) IO a

vLift = id
rLift = lift
tLift = lift . lift

class RenderContext c where
    type Rendering c :: *

    renderD :: Document -> Renderer c (Rendering c)

    renderB :: Block -> Renderer c (Rendering c)
    renderB b@(Paragraph _) = renderP b

    renderP :: Block -> Renderer c (Rendering c)

    renderI :: Inline -> Renderer c (Rendering c)
    renderI i@(Content _) = renderC i
    renderI i@(Transform _ _) = renderT i

    renderC :: Inline -> Renderer c (Rendering c)
    renderT :: Inline -> Renderer c (Rendering c)

runRendererT :: Monad m => RenderContext c => v -> c -> t -> RendererT v c t m (Rendering c) -> m (Rendering c)
runRendererT v c t = flip evalStateT t . flip evalStateT c . flip evalStateT v

runRenderer :: RenderContext c => VerseState c -> c -> Renderer c (Rendering c) -> IO (Rendering c)
runRenderer ts c = runRendererT ts c (TransformContext ())

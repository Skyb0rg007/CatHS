{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Text.Prettyprint.Doc.Render.HTML
    ( renderHtml
    ) where

import           Data.Foldable                                       (traverse_)
import           Data.Map                                            (Map)
import           Data.Text                                           (Text)
import qualified Data.Text                                           as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import           Reflex.Dom

renderHtml :: forall t m . DomBuilder t m => SimpleDocStream (Map Text Text) -> m ()
renderHtml sdoc = el "pre" $ go (treeForm sdoc)
    where
        go = \case
            STEmpty        -> pure ()
            STChar c       -> text $ Text.singleton c
            STText _ t     -> text t
            STLine i       -> text $ "\n" <> Text.replicate i (Text.singleton ' ')
            STAnn ann rest -> elAttr "span" ann $ go rest
            STConcat xs    -> traverse_ go xs




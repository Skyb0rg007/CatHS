{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import           Control.Lens
import           Control.Monad             (void, forM_)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Fix
import           Data.ByteString           (ByteString)
import           Data.Default              (def)
import           Data.FileEmbed            (embedFile)
import           Data.Map                  (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Time.Clock           (getCurrentTime)
import           Reflex
import           Reflex.Dom
import           Text.Megaparsec           (errorBundlePretty, parse)
import           Data.List (intersperse)

import           Cat.Parse
import           Cat.Syntax
import           Data.Text.Prettyprint.Doc.Render.HTML

main :: IO ()
main = mainWidgetWithCss css app

css :: ByteString
css = mconcat $ intersperse "\n" $
    [ normalizeCss
    , "body {"
    , "  background-color: #eee8d5;"
    , "  max-width: 1200px;"
    , "  margin: 0px auto;"
    , "  padding: 5px;"
    , "  color: #073642;"
    , "}"
    , "h1, p {"
    , "  font-family: sans-serif;"
    , "  padding: 0px 10px;"
    , "}"
    , "#left, #right {"
    , "  width: 50%;"
    , "  height: 100%;"
    , "  padding: 10px;"
    , "  box-sizing: border-box;"
    , "}"
    , "#left { float: left; }"
    , "#right { float: right; }"
    , "#content {"
    , "  width: 100%;"
    , "  padding: 0px;"
    , "  height: 450px;"
    , "}"
    , "#content::after { content: \".\"; visibility: hidden; display: block; height: 0; clear: both; }"
    , "#input {"
    , "  width: 100%;"
    , "  height: 100%;"
    , "  box-sizing: border-box;"
    , "  padding: 10px;"
    , "  resize: none;"
    , "  outline: none;"
    , "}"
    ]

normalizeCss :: ByteString
normalizeCss = $(embedFile "normalize.css")

app :: forall m t .
    ( PerformEvent t m
    , MonadHold t m
    , MonadIO (Performable m)
    , DomBuilder t m
    , PostBuild t m
    , MonadFix m
    )
    => m ()
app = do
    el "p" $ do
        text "Enter text and see how it parses!"
        el "br" (pure ())
        el "b" $ text "NOTE: The pretty-printer removes all unnecessary parentheses"
    elAttr "div" ("id"=:"content") $ do
        inp <- elAttr "div" ("id"=:"left") codeInput
        postBuild <- getPostBuild
        let inp' = updated inp <> tag (current inp) postBuild
        elAttr "div" ("id"=:"right") $ parsedOutput inp'
    el "p" $ do
        text "Legend: ["
        forM_ [AnnInt .. AnnFun] $ \ann -> do
            elAttr "span" (mapAnn ann) $ text $ Text.pack (drop 3 (show ann))
            text ", "
        text "]"

parsedOutput :: forall t m . ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m )
             => Event t Text
             -> m ()
parsedOutput changeTxt = void $ dyn =<< fmap render <$> foldDyn f ([], Nothing) changeTxt
    where
        f :: Text -> (Program, Maybe Text) -> (Program, Maybe Text)
        f newTxt (oldProg, oldErr) =
            case parse parseProgram "" newTxt of
              Left err -> (oldProg, Just $ Text.pack (errorBundlePretty err))
              Right prog -> (prog, Nothing)
        render :: (Program, Maybe Text) -> m ()
        render (prog, mErr) = do
            renderHtml $ layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine 50 0.4 } $ mapAnn <$> prettyProg prog
            case mErr of
              Nothing -> pure ()
              Just err -> el "hr" (pure ()) >> elAttr "pre" ("style"=:"color: red") (text err)

mapAnn :: ExpAnn -> Map Text Text
mapAnn = \case
    AnnInt     -> "style"=:"color: #E72F06"
    AnnString  -> "style"=:"color: #E74706"
    AnnOp      -> "style"=:"color: #4B9834"
    AnnKeyword -> "style"=:"color: #4B9834"
    AnnVar     -> "style"=:"color: #0B98AD"
    AnnTy      -> "style"=:"color: #E7B206"
    AnnField   -> "style"=:"color: #8C29A2"
    AnnFun     -> "style"=:"color: #0B98AD"

codeInput :: forall t m .  ( DomBuilder t m )
          => m (Dynamic t Text)
codeInput = do
    te <- textAreaElement $
        def & textAreaElementConfig_initialValue .~ "type intArray = array of int\nfunction main () -> void {\n  print(\"Hello!\")\n}"
            & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
                ("placeholder"=:"Enter code" <> "id"=:"input" <> "spellcheck"=:"false")
    pure $ _textAreaElement_value te
        


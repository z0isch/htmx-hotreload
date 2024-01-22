{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (app) where

import Data.Foldable (sequenceA_)
import Lucid
import Servant
import Servant.HTML.Lucid

type API = Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

app :: Maybe (Html ()) -> Application
app = serve api . server

server :: Maybe (Html ()) -> Handler (Html ())
server mHotreload = pure $ html_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
        title_ "Hotreload Test"
        sequenceA_ mHotreload
    body_ $ do
        iframe_
            [src_ "https://www.youtube.com/embed/dQw4w9WgXcQ?si=vmk7Ow-_dGCXpRLj"]
            ""
        h1_ "Rick roll!"

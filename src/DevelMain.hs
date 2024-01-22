{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DevelMain (update) where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (race_)
import Data.Text (Text)
import Lib (app)
import Lucid (Html, script_, src_)
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ControlMessage (..), Message (..), acceptRequest, defaultConnectionOptions, receive, sendTextData)
import Rapid (createRef, rapid, restart, start)
import Text.Shakespeare.Text (st)

update :: IO ()
update =
    rapid 0 $ \r -> do
        hotreloadSem <- createRef @Text r "hotreloadSem" $ newEmptyMVar @()

        start r "hotreload" $ run 8081 $ hotReloadServer hotreloadSem

        restart r "webserver" $ do
            _ <- tryPutMVar hotreloadSem ()
            run 8080 $ app $ Just $ hotreloadJs "ws://localhost:8081"

hotReloadServer :: MVar () -> Application
hotReloadServer hotreloadSem = websocketsOr defaultConnectionOptions hotreloader backup
  where
    hotreloader pc = do
        c <- acceptRequest pc
        _ <- tryPutMVar hotreloadSem ()
        let
            handleClose =
                receive c >>= \case
                    ControlMessage (Close _ _) -> pure ()
                    _ -> handleClose
            hotreload = do
                takeMVar hotreloadSem
                sendTextData @Text c "hotreload"
                hotreload
        race_ handleClose hotreload
    backup _ resp = resp $ responseLBS status400 [] "Not a WebSocket request"

hotreloadJs :: Text -> Html ()
hotreloadJs uri = do
    script_ [src_ "https://unpkg.com/idiomorph@0.3.0"] ("" :: String)
    script_
        [st|
(function () {
  let timeout = 1000;
  const resetBackoff = () => {
    timeout = 1000;
  };

  const backOff = () => {
    if (timeout > 10 * 1000) {
      return;
    }

    timeout = timeout * 2;
  };

  let init = true
  function connectHotReload() {
    const socket = new WebSocket("#{uri}");

    socket.onmessage = async (e) => {
        if(!init){
            Idiomorph.morph(document.documentElement,await (await fetch(location.href)).text())
        }
        else{
            init = false
        }
    };

    socket.onopen = () => {
      resetBackoff();
    };

    socket.onclose = () => {
      const timeoutId = setTimeout(function () {
        clearTimeout(timeoutId);
        backOff();

        connectHotReload();
      }, timeout);
    };
  }

  connectHotReload();
})();
|]

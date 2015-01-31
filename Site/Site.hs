{-# LANGUAGE OverloadedStrings #-}
module Site.Site where

import qualified Data.Vector as V
import Control.Concurrent.MVar
import Control.Applicative
import Animations.LED
import Site.Animations
import Site.RootPage
import Control.Monad.IO.Class
import Snap.Core
import Text.Blaze.Html.Renderer.Utf8
import Snap.Util.FileServe
import qualified Data.ByteString.Lazy.Char8 as BS

--Things we need to hold info for:
-- 1 The actual annimation (animList)
--       :: V.Vector (Animation,BlendingMode)
-- 2 What animations are available, and the types/ranges of their arguments
--       :: [AvailAnim]
-- 3 What animations are active, and the values of their arguments
--       :: [AnimMetadata]

site :: MVar (V.Vector (Animation,BlendingMode)) -> Snap ()
site m = do
        animmeta <- liftIO $ newMVar []
        ifTop (rootHandler animmeta)
            <|> route [ ("foo", writeBS "bar")
                      , ("echo/:echoparam", writeBS "lol")
                      ]
            <|> dir "static" (serveDirectory ".")

rootHandler :: MVar [AnimMetadata] -> Snap ()
rootHandler animmeta = do curranims <- liftIO $ readMVar animmeta
                          writeBS
                                $ BS.toStrict
                                $ renderHtml
                                $ rootPage curranims

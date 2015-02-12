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
import Data.Map.Lazy

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
            <|> route [ ("newanims", newAnims animmeta)
                      ]
            <|> dir "static" (serveDirectory "static")

rootHandler :: MVar [AnimMetadata] -> Snap ()
rootHandler animmeta = do curranims <- liftIO $ readMVar animmeta
                          writeBS
                                $ BS.toStrict
                                $ renderHtml
                                $ rootPage curranims

newAnims :: MVar [AnimMetadata] -> Snap ()
newAnims mAnimMeta = do req <- getRequest
                        let postParams = rqPostParams req
                        if member "newanims" postParams
                            then do let jsonblob = postParams ! "newanims"
                                    return ()
                            else modifyResponse $ setResponseCode 500

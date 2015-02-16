{-# LANGUAGE OverloadedStrings #-}
module Site.Site where

import qualified Data.Vector as V
import Control.Concurrent.MVar
import Control.Applicative
import Animations.LED
import Site.RootPage
import Site.JSON
import Control.Monad.IO.Class
import Snap.Core
import Text.Blaze.Html.Renderer.Utf8
import Snap.Util.FileServe
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as Map

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
        ifTop rootHandler
            <|> route [ ("newanims", newAnims m animmeta)
                      ]
            <|> dir "static" (serveDirectory "static")

rootHandler :: Snap ()
rootHandler = writeBS $ BSL.toStrict $ renderHtml rootPage

newAnims :: MVar (V.Vector (Animation,BlendingMode))
         -> MVar [AnimMetadata]
         -> Snap ()
newAnims anims mAnimMeta = do req <- getRequest
                              let postParams = rqPostParams req
                              if Map.member "newanims" postParams
                                  then do let jsonblob = postParams Map.! "newanims"
                                              jsonanims = jsonblob !! 0
                                              newanims = parseJSON $ BS.unpack jsonanims
                                          liftIO $ modifyMVar_ mAnimMeta (\_ -> return newanims)
                                          liftIO $ modifyMVar_ anims (\_ -> return $ V.fromList $ metaToAnims newanims)
                                          liftIO $ putStrLn $ "New Anims: " ++ show newanims
                                  else modifyResponse $ setResponseCode 500

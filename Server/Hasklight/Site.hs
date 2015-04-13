{-# LANGUAGE OverloadedStrings #-}
module Hasklight.Site where

import Control.Concurrent.MVar
import Control.Applicative
import Text.JSON
import Text.JSON.Generic
import Control.Monad.IO.Class
import Snap.Core
import Text.Blaze.Html.Renderer.Utf8
import Snap.Util.FileServe
import System.Directory
import System.FilePath.Posix
import Hasklight.LED

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8      as BS
import qualified Data.Map.Lazy              as Map
import qualified Data.Vector                as V

import Hasklight.AnimParams
import Hasklight.JSON

site :: MVar (V.Vector (Animation,BlendingMode))
     -> MVar [AnimMetadata]
     -> String
     -> FilePath
     -> Snap ()
site m animmeta host presetsdir = do
        route [ ("set", setAnims m animmeta)
              , ("get", getAnims animmeta)
              , ("list", listAnims)
              , ("getpresets", getPresets presetsdir)
              , ("getpreset/:name", getPreset presetsdir)
              , ("setpreset/:name", setPreset presetsdir)
              , ("delpreset/:name", delPreset presetsdir)
              ]
          <|> dir "static" (serveDirectory "static")

lookAtPresets :: String -> IO [String]
lookAtPresets presetsdir = filter (\x -> x /= "." && x /= "..")
                               `fmap` getDirectoryContents presetsdir

--Sets the active animations
setAnims :: MVar (V.Vector (Animation,BlendingMode))
         -> MVar [AnimMetadata]
         -> Snap ()
setAnims anims animmeta = do
    req <- getRequest
    let postParams = rqPostParams req
    if Map.member "newanims" postParams
        then do let jsonblob = postParams Map.! "newanims"
                    jsonanims = jsonblob !! 0
                    newanims = decodeJSON $ BS.unpack jsonanims
                liftIO $ modifyMVar_ animmeta (\_ -> return newanims)
                liftIO $ modifyMVar_ anims
                            (\_ -> V.fromList `fmap` mapM convert newanims)
                liftIO $ putStrLn $ "New Anims: " ++ show newanims
        else modifyResponse $ setResponseCode 500

--Get the currently active animations
getAnims :: MVar [AnimMetadata] -> Snap ()
getAnims animmeta = do
    anims <- liftIO $ readMVar animmeta
    writeBS $ BS.pack $ encodeJSON anims

--Lists the available animations
listAnims :: Snap ()
listAnims = writeBS $ BS.pack $ encodeJSON availAnims

--Lists the available presets
getPresets :: FilePath -> Snap ()
getPresets presetsdir = do
    presets <- liftIO $ lookAtPresets presetsdir
    writeBS $ BS.pack $ encode presets

--Sets a preset to the given JSON
setPreset :: FilePath -> Snap ()
setPreset presetsdir = do
    mname <- getParam "name"
    postParams <- rqPostParams `fmap` getRequest
    case mname of
        Just n-> if Map.member "animations" postParams
                     then do let jsonblob = postParams Map.! "animations"
                                 p = jsonblob !! 0
                             liftIO $ BS.writeFile (joinPath [presetsdir,BS.unpack n]) p
                             writeBS "Success!"
                     else writeBS "Missing post parameter: animations"
        Nothing   -> writeBS "You need to specify a name. /setpreset/:name"

--Gets the JSON for a given preset
getPreset :: FilePath -> Snap ()
getPreset presetsdir = do
    mname <- getParam "name"
    case mname of
        Just n -> do p <- liftIO $ BS.readFile (joinPath [presetsdir,BS.unpack n])
                     writeBS p
        Nothing   -> writeBS "You need to specify a name. /getpreset/:name"

--Deletes a given preset
delPreset :: FilePath -> Snap ()
delPreset presetsdir = do
    mname <- getParam "name"
    case mname of
        Just n -> do liftIO $ removeFile (joinPath [presetsdir,BS.unpack n])
                     writeBS "Success!"
        Nothing   -> writeBS "You need to specify a name. /delpreset/:name"

module Hasklight.Client.Utils where

import Control.Monad               ( filterM )
import Data.Maybe                  ( fromJust
                                   , isJust
                                   )
import GHCJS.DOM.Document          ( documentGetElementsByClassName )
import GHCJS.DOM.Node              ( IsNode
                                   , nodeContains
                                   )
import GHCJS.DOM.NodeList          ( NodeList )
import GHCJS.DOM.NodeList          ( nodeListItem 
                                   , nodeListGetLength
                                   )
import GHCJS.DOM.Types             ( Document
                                   , Node
                                   )
import Language.Javascript.JSaddle ( JSM
                                   , JSValueRef
                                   )

type JSEnv = (JSM JSValueRef -> IO ())

getChildrenByClass :: (IsNode par) => Document -> String -> par -> IO ([Node])
getChildrenByClass doc tag par = do
        mnodes <- documentGetElementsByClassName doc tag
        case mnodes of
            Just nodes -> do 
                mns <- toList nodes
                filterM (nodeContains par . Just) mns
            Nothing -> return []

toList :: NodeList -> IO [Node]
toList ns = do l <- nodeListGetLength ns
               if l > 0
                   then mapM (nodeListItem ns) [0..(l-1)]
                            >>= filterM (return . isJust)
                            >>= mapM (return . fromJust)
                   else return []

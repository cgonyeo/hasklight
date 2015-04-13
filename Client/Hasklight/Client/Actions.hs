module Hasklight.Client.Actions where

import GHCJS.DOM.Document          ( documentCreateElement
                                   , documentGetElementsByClassName
                                   , documentGetElementById
                                   )
import GHCJS.DOM.Element           ( elementOnclick
                                   , elementGetClassList
                                   )
import GHCJS.DOM.HTMLElement       ( htmlElementGetInnerText
                                   , htmlElementSetInnerHTML
                                   , IsHTMLElement
                                   )
import GHCJS.DOM.Node              ( nodeAppendChild
                                   , nodeGetParentNode
                                   , nodeRemoveChild
                                   )
import GHCJS.DOM.NodeList          ( nodeListItem 
                                   , nodeListGetLength
                                   )
import GHCJS.DOM.Types             ( castToHTMLAnchorElement
                                   , castToHTMLButtonElement
                                   , castToHTMLDivElement
                                   , castToHTMLHeadingElement
                                   , Document
                                   , HTMLButtonElement
                                   )
import Control.Applicative         ( (<$>) )
import Control.Monad               ( filterM )
import Control.Monad.IO.Class      ( liftIO )
import Data.Text.Lazy              ( unpack )
import Language.Javascript.JSaddle ( eval )

import Hasklight.AnimParams
import Hasklight.Client.Rendering
import Hasklight.Client.Utils


addAnimAction :: JSEnv -> Document -> Int -> IO ()
addAnimAction runjs doc i = do
        Just animationsdiv  <- fmap castToHTMLDivElement
                             <$> documentGetElementById doc "animationsdiv"
        Just anim <- fmap castToHTMLAnchorElement
                            <$> documentGetElementById doc ("avail-" ++ show i)
        elementOnclick anim . liftIO $ do
            let anim = renderAnim (availAnims !! i)
            Just newdiv <- fmap castToHTMLDivElement
                                    <$> documentCreateElement doc "div"
            htmlElementSetInnerHTML newdiv (unpack anim)
            nodeAppendChild animationsdiv (Just newdiv)
            delAnimAction doc newdiv
            setupColorJs runjs
            return ()
        return ()

delAnimAction :: IsHTMLElement self => Document -> self -> IO ()
delAnimAction doc self = do
        btns <- getChildrenByClass doc "del-anim" self
        elementOnclick (castToHTMLButtonElement (last btns)) . liftIO $ do
            mpar <- nodeGetParentNode self
            case mpar of
                Just par -> do nodeRemoveChild par (Just self)
                               return ()
                Nothing -> putStrLn "Error getting self's parent for removal"
        return ()

setupColorJs :: JSEnv -> IO ()
setupColorJs runjs = do
        runjs $ eval $ "$('.colopt').last().ColorPickerSliders({"
                     ++     "size: 'sm',"
                     ++     "placement: 'bottom',"
                     ++     "swatches: false,"
                     ++     "order: {"
                     ++         "hsl: 1"
                     ++     "}"
                     ++ "});"

applyAnimAction :: Document -> HTMLButtonElement -> IO ()
applyAnimAction doc okbtn = do
        elementOnclick okbtn . liftIO $ do
            manimdivs <- documentGetElementsByClassName doc "animdata"
            case manimdivs of
                Just animdivs -> do
                    animdivs' <- toList animdivs
                    putStrLn $ "We currently have "
                                    ++ show (length animdivs')
                                    ++ " animations"
                    names <- ( (map castToHTMLHeadingElement)
                             . (map (!! 0))
                             . (filter ((> 0) . length))
                             ) `fmap` mapM (getChildrenByClass doc "name") animdivs'
                                >>= (mapM (htmlElementGetInnerText :: IsHTMLElement h => h -> IO String))
                    vals  <- mapM (getChildrenByClass doc "opt") animdivs'
                    print names
                    return ()
                Nothing -> putStrLn "Error getting animdata"
        return ()

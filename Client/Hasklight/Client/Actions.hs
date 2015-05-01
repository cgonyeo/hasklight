module Hasklight.Client.Actions where

import GHCJS.DOM.Document          ( documentCreateElement
                                   , documentGetElementsByClassName
                                   , documentGetElementById
                                   )
import GHCJS.DOM.Element           ( elementGetAttribute
                                   , elementGetClassList
                                   , elementOnclick
                                   )
import GHCJS.DOM.HTMLElement       ( htmlElementGetInnerText
                                   , htmlElementSetInnerHTML
                                   , IsHTMLElement
                                   )
import GHCJS.DOM.HTMLInputElement  ( htmlInputElementGetValue )
import GHCJS.DOM.HTMLSelectElement ( htmlSelectElementGetValue )
import GHCJS.DOM.Node              ( nodeAppendChild
                                   , nodeGetParentNode
                                   , nodeRemoveChild
                                   )
import GHCJS.DOM.NodeList          ( NodeList
                                   , nodeListItem 
                                   , nodeListGetLength
                                   )
import GHCJS.DOM.Types             ( castToHTMLAnchorElement
                                   , castToHTMLButtonElement
                                   , castToHTMLDivElement
                                   , castToHTMLHeadingElement
                                   , castToHTMLInputElement
                                   , castToHTMLLabelElement
                                   , castToHTMLSelectElement
                                   , Document
                                   , HTMLButtonElement
                                   , HTMLInputElement
                                   , Node
                                   )
import Control.Applicative         ( (<$>) )
import Control.Monad               ( filterM )
import Control.Monad.IO.Class      ( liftIO )
import Data.List.Split             ( splitWhen )
import Data.Maybe                  ( catMaybes )
import Data.Text.Lazy              ( unpack )
import Language.Javascript.JSaddle ( eval
                                   , JSM
                                   , JSValueRef
                                   )
import Text.Read                   ( readMaybe )

import Hasklight.AnimMetadata
import Hasklight.Client.Rendering
import Hasklight.Client.Utils
import Hasklight.Convert


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

applyAnimAction :: Document -> JSEnv -> HTMLButtonElement -> IO ()
applyAnimAction doc runjs okbtn = do
    elementOnclick okbtn . liftIO $ do
        manimdivs <- documentGetElementsByClassName doc "animdata"
        case manimdivs of
            Just animdivs -> do
                animdivs' <- toList animdivs
                names <- mapM (getChildrenByClass doc "name") animdivs'
                         >>= ( (mapM htmlElementGetInnerText)
                             . (map castToHTMLHeadingElement)
                             . (map (!! 0))
                             . (filter ((> 0) . length))
                             )
                vals  <- do fieldtags   <- mapM (getChildrenByClass doc "opt") animdivs'
                            fields <- mapM (mapM (processField . castToHTMLInputElement)) fieldtags
                            nametags <- mapM (getChildrenByClass doc "field-name") animdivs'
                            names <- mapM (mapM (htmlElementGetInnerText . castToHTMLLabelElement)) nametags
                            let fields' = map catMaybes fields
                            return $ zipWith (zipWith AnimParam) names fields'
                modes <- mapM (getChildrenByClass doc "anim-bl") animdivs'
                            >>= ( mapM htmlSelectElementGetValue
                                . map castToHTMLSelectElement
                                . map (!!0)
                                . filter ((> 0) . length)
                                )
                let json = writeJSON (zipWith3 AnimMetadata names vals modes)
                runjs $ eval $ "$.post(\"/set\", { \"newanims\" : "
                                    ++ show json ++ "});"
            Nothing -> putStrLn "Error getting animdata"
    return ()

processField :: HTMLInputElement -> IO (Maybe AnimField)
processField i = do
        attr <- elementGetAttribute i "opttype"
        val <- htmlInputElementGetValue i
        case attr of
            "double" -> case readMaybe val of
                            Just d -> return . Just $ AnimDouble d
                            Nothing -> return Nothing
            "int" -> case readMaybe val of
                         Just n -> return . Just $ AnimInt n
                         Nothing -> return Nothing
            "color" -> (return $ readColor val)
            a -> putStrLn ("Unrecognized attr: " ++ a) >> return Nothing

readColor :: String -> Maybe AnimField
readColor [] = Nothing
readColor str = let toks = Just . splitWhen (==',') . init $ drop 4 str
                    f [sr,sg,sb] = Just (readMaybe sr,readMaybe sg,readMaybe sb)
                    f _          = Nothing
                    g (Just r,Just g,Just b) = Just $ AnimLED r g b
                    g _                      = Nothing
                in toks >>= f >>= g
